package de.bwhc.mtb.broker.connector


import java.net.URL
import scala.util.{Success,Failure}
import scala.concurrent.{
  ExecutionContext,
  Future
}
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.ws.{
  StandaloneWSClient,
  StandaloneWSRequest
}
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.api.libs.ws.{
  StandaloneWSRequest => WSRequest,
  StandaloneWSResponse => WSResponse
}
import play.api.libs.ws.JsonBodyReadables._
import play.api.libs.ws.JsonBodyWritables._
import play.api.libs.ws.DefaultBodyWritables._
import play.api.libs.ws.DefaultBodyReadables._
import play.api.libs.json.{Json,JsValue}
import cats.data.{Ior,IorNel,NonEmptyList}
import cats.syntax.either._
import cats.syntax.ior._
import cats.instances.list._
import de.bwhc.util.Logging
import de.bwhc.util.json._
import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICD10GM,
  Medication,
  MTBFile,
  ZPM
}
import de.bwhc.mtb.query.api.{
  Querier,
  Query,
  PeerStatus,
  PeerStatusReport,
  PeerToPeerRequest,
  MTBFileParameters,
  ConceptCount,
  LocalQCReport,
  Report,
  LocalReport,
  Snapshot,
  PatientTherapies,
}
import de.bwhc.mtb.query.api.ReportingAliases._
import de.bwhc.mtb.query.impl.{
  BwHCConnector,
  BwHCConnectorProvider
}



class BwHCConnectorProviderImpl extends BwHCConnectorProvider
{

  def getInstance: BwHCConnector = {
    BwHCConnectorImpl.instance
  }

}


object BwHCConnectorImpl
{

  implicit val system       = ActorSystem()
  implicit val materializer = Materializer.matFromSystem

  private val wsclient = StandaloneAhcWSClient()
  private val config   = Config.getInstance

  val instance = new BwHCConnectorImpl(wsclient,config)

}

class BwHCConnectorImpl
(
  private val wsclient: StandaloneWSClient,
  private val config: Config
)
extends BwHCConnector
with Logging
{

  private val timeout = 10 seconds

  private val OK = 200

  private val baseUri = "/bwhc/peer2peer/api"

  // Set-up for periodic auto-update of peer config

  import java.time.{Duration,LocalTime}
  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS
  import java.util.concurrent.atomic.AtomicReference


  private def sitesConfig: Future[List[Sites.Entry]] = {

    import ExecutionContext.Implicits.global

    log.debug(s"Requesting peer connectivity config")

    request("/sites")
      .withRequestTimeout(timeout)
      .get()
      .map(_.body[JsValue].as[Sites])
      .map {
        case Sites(sites) =>
          sites.filterNot(_.id == config.siteId)
      }
      .recover {
        case t =>
          log.error(s"Broker connection error: ${t.getMessage}")
          List.empty[Sites.Entry]
      }

  }

  private val siteList: AtomicReference[Future[List[Sites.Entry]]] =
    new AtomicReference(sitesConfig)

  private val executor =
    Executors.newSingleThreadScheduledExecutor

  for { period <- config.updatePeriod }{

    executor.scheduleAtFixedRate(
      () => siteList.set(sitesConfig),
      period,
      period,
      SECONDS
    )
  }


  private def request(
    rawUri: String,
    virtualHost: Option[String] = None
  ): WSRequest = {

    val uri =
      if (rawUri startsWith "/") rawUri.substring(1)
      else rawUri

    val req =
      wsclient.url(s"${config.baseURL}$uri")
        .withRequestTimeout(timeout)

    virtualHost match {
      case Some(host) => req.withHttpHeaders("Host" -> host)
      case _          => req
    }
  }


  private def scatter(
    uri: String
  )(
    implicit ec: ExecutionContext
  ): Future[List[(String,WSRequest)]] = {
    
    for {
      sites <- siteList.get

      requests =
        sites.map {
          case Sites.Entry(_,name,virtualHost) =>
            name -> request(uri,Some(virtualHost))
        }
    } yield requests

  }


  private def gather[T,U](
    responses: List[Future[T]]
  )(
    acc: U
  )(
    f: (U,T) => U
  )(
    implicit ec: ExecutionContext
  ): Future[U] =
    Future.foldLeft(responses)(acc)(f)



  private def scatterGather[T,U](
    uri: String
  )(
    trf: (String,WSRequest) => Future[T]
  )(
    acc: U
  )(
    aggr: (U,T) => U
  )(
    implicit ec: ExecutionContext
  ): Future[U] =
    for {
      requests <- scatter(uri)
      results  =  requests.map(trf.tupled)
      result   <- gather(results)(acc)(aggr)
    } yield result



  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = {

    import PeerStatus._

    scatterGather(s"$baseUri/status")(
      (site,request) =>
        request
          .get()
          .map(
            response =>
              if (response.status == OK)
                PeerStatusReport.Info(ZPM(site),Online,"-")
              else
                PeerStatusReport.Info(ZPM(site),Offline,response.body[String])
          )
          .recover {
            case t => PeerStatusReport.Info(ZPM(site),Offline,t.getMessage)
          }
    )(
      List.empty[PeerStatusReport.Info]
    )(
      _ :+ _
    )
    .map(st => PeerStatusReport(peerStatus = st))

  }


  override def requestQCReports(
    p2pRequest: PeerToPeerRequest[Map[String,String]]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

    log.debug(
      s"Requesting LocalQCReports from peers for Querier ${p2pRequest.querier.value}"
    )

    scatterGather(s"$baseUri/LocalQCReport")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalQCReport]) //TODO: handle validation errors
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t => 
              s"Error in LocalQCReport response from site $site: ${t.getMessage}".leftIor[LocalQCReport].toIorNel
          }
          .map(_.map(List(_)))
    )(
      List.empty[LocalQCReport].rightIor[String].toIorNel
    )(
      _ combine _
    )

  }


  override def requestMedicationDistributionReports(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalDistributionReport[Medication.Coding]]]] = {

    log.debug(s"Executing Peer-to-Peer requests for Medication Distribution Reports")

    scatterGather(s"$baseUri/medication-distribution")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalReport[Seq[ConceptCount[Medication.Coding]]]])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t =>
              s"Error in Peer-to-peer request from bwHC Site $site: ${t.getMessage}".leftIor[LocalDistributionReport[Medication.Coding]].toIorNel
          }
    )(
      List.empty[LocalDistributionReport[Medication.Coding]].rightIor[String].toIorNel
    )(
      _ combine _.map(List(_))
    )

  }


  override def requestTumorEntityDistributionReports(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalDistributionReport[Coding[ICD10GM]]]]] = {

    log.debug(s"Executing Peer-to-Peer requests for TumorEntity Distribution Reports")

    scatterGather(s"$baseUri/tumor-entity-distribution")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalReport[Seq[ConceptCount[Coding[ICD10GM]]]]])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t =>
              s"Error in Peer-to-peer request from bwHC Site $site: ${t.getMessage}".leftIor[LocalDistributionReport[Coding[ICD10GM]]].toIorNel
          }
    )(
      List.empty[LocalDistributionReport[Coding[ICD10GM]]].rightIor[String].toIorNel
    )(
      _ combine _.map(List(_))
    )

  }


  override def requestPatientTherapies(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalReport[Seq[PatientTherapies]]]]] = {

    log.debug(s"Executing Peer-to-Peer requests for PatientTherapies")

    scatterGather(s"$baseUri/patient-therapies")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalReport[Seq[PatientTherapies]]])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t =>
              s"Error in Peer-to-peer request from bwHC Site $site: ${t.getMessage}".leftIor[LocalReport[Seq[PatientTherapies]]].toIorNel
          }
    )(
      List.empty[LocalReport[Seq[PatientTherapies]]].rightIor[String].toIorNel
    )(
      _ combine _.map(List(_))
    )

  }


  override def executeQuery(
    q: PeerToPeerRequest[Query.Parameters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

    log.debug(s"Executing Peer-to-Peer Query ${q}") //TODO: Query to JSON

    scatterGather(s"$baseUri/query")(
      (site,request) =>
        request
          .post(Json.toJson(q))
          .map(_.body[JsValue].as[SearchSet[Snapshot[MTBFile]]]) //TODO: handle validation errors
           .map(_.entries) 
           .map(_.rightIor[String].toIorNel)
           .recover {
             case t => 
               s"Error in Peer-to-peer Query response from site $site: ${t.getMessage}".leftIor[List[Snapshot[MTBFile]]].toIorNel
           }
    )(
      List.empty[Snapshot[MTBFile]].rightIor[String].toIorNel
    )(
      _ combine _
    )

  }


  override def requestMTBFile(
    site: ZPM,
    mfreq: PeerToPeerRequest[MTBFileParameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,Option[Snapshot[MTBFile]]]] = {

    log.debug(s"Executing MTBFile Snapshot Request ${mfreq}") //TODO: Query to JSON

    for {
      sites <- siteList.get

      result <-
        sites.find(_.name == site.value) match {

          case None =>
            Future.successful(
              s"Invalid site name ${site.value}; should be one of [${sites.map(_.name).mkString(", ")}]".asLeft[Option[Snapshot[MTBFile]]]
            )

          case Some(Sites.Entry(id,name,virtualHost)) => {

            log.debug(s"Site: $name")

            request(s"$baseUri/MTBFile:request",Some(virtualHost))
              .post(Json.toJson(mfreq))
              .map {
                resp =>
                  resp.status match {
                    case 200 => Some(resp.body[JsValue].as[Snapshot[MTBFile]])
                    case 404 => None
                  }
              } //TODO: handle validation errors
              .map(_.asRight[String])
              .recover {
                case t => 
                  s"Error in MTBFile retrieval request from site $name: ${t.getMessage}".asLeft[Option[Snapshot[MTBFile]]]
              }
      
          }
        }

    } yield result

  }

}
