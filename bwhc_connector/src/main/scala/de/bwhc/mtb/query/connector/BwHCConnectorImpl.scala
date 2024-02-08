package de.bwhc.mtb.query.connector


import java.net.URL
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
import cats.data.{Ior,IorNel}
import cats.syntax.ior._
import cats.syntax.either._
import cats.instances.list._
import de.bwhc.util.Logging
import de.bwhc.mtb.dtos.{
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
  LocalQCReport,
  ConceptCount,
  LocalReport,
  Report,
  PatientTherapies,
  Snapshot
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

  private val timeout =
    config.timeout.getOrElse(10) seconds

  private val OK = 200


  private def request(
    baseUrl: URL,
    rawUri: String
  ): WSRequest = {

    val uri =
      if (rawUri startsWith "/") rawUri.substring(1)
      else rawUri

    wsclient.url(s"${baseUrl}$uri")
      .withRequestTimeout(timeout)
  }


  private def scatter(
    uri: String
  )(
    implicit ec: ExecutionContext
  ): List[(ZPM,WSRequest)] = {

    for {
      (site,baseUrl) <- config.peerBaseURLs.toList
    } yield {
      site -> request(baseUrl,uri)
    }
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
    trf: (ZPM,WSRequest) => Future[T]
  )(
    acc: U
  )(
    aggr: (U,T) => U
  )(
    implicit ec: ExecutionContext
  ): Future[U] = {

    import scala.util.chaining._

    scatter(uri)
      .map(trf.tupled) pipe (
        res => gather(res)(acc)(aggr)
      )

  }

  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = {

    import PeerStatus._

    log.info("Checking connection status to peers")

    scatterGather("status")(
      (site,request) =>
        request
          .get()
          .map(
            response =>
              if (response.status == OK)
                PeerStatusReport.Info(site,Online,"-")
              else {
                val msg = response.body[String]
                log.error(s"Problem in peer-to-peer status check of site '$site': $msg")
                PeerStatusReport.Info(site,Offline,msg)
              }
          )
          .recover {
            case t =>
              log.error(s"Problem in peer-to-peer status check of site '$site'",t)
              PeerStatusReport.Info(site,Offline,t.getMessage)
          }
    )(
      List.empty[PeerStatusReport.Info]
    )(
      _ :+ _
    )
    .map(st => PeerStatusReport(peerStatus = st)) 
  }



  override def requestQCReports(
//    p2pRequest: PeerToPeerRequest[Naught]
    p2pRequest: PeerToPeerRequest[Map[String,String]]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

    log.debug(s"Requesting LocalQCReports from peers for Querier ${p2pRequest.querier.value}")

    scatterGather("LocalQCReport")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalQCReport])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t => 
              s"Error in LocalQCReport response from bwHC Site ${site.value}: ${t.getMessage}".leftIor[LocalQCReport].toIorNel
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

    scatterGather("medication-distribution")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalReport[Seq[ConceptCount[Medication.Coding]]]])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t => 
              s"Error in Peer-to-peer request from bwHC Site ${site.value}: ${t.getMessage}".leftIor[LocalDistributionReport[Medication.Coding]].toIorNel
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

    scatterGather("tumor-entity-distribution")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalReport[Seq[ConceptCount[Coding[ICD10GM]]]]])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t => 
              s"Error in Peer-to-peer request from bwHC Site ${site.value}: ${t.getMessage}".leftIor[LocalDistributionReport[Coding[ICD10GM]]].toIorNel
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

    scatterGather("patient-therapies")(
      (site,request) =>
        request
          .post(Json.toJson(p2pRequest))
          .map(_.body[JsValue].as[LocalReport[Seq[PatientTherapies]]])
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t => 
              s"Error in Peer-to-peer request from bwHC Site ${site.value}: ${t.getMessage}".leftIor[LocalReport[Seq[PatientTherapies]]].toIorNel
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

    scatterGather("query")(
      (site,request) =>
        request
          .post(Json.toJson(q))
          .map(_.body[JsValue].as[SearchSet[Snapshot[MTBFile]]]) //TODO: handle validation errors
          .map(_.entries) 
          .map(_.rightIor[String].toIorNel)
          .recover {
            case t => 
              s"Error in Peer-to-peer Query response from bwHC Site ${site.value}: ${t.getMessage}".leftIor[List[Snapshot[MTBFile]]].toIorNel
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

    config.peerBaseURLs.get(site) match {

      case None =>
        Future.successful(
          s"Invalid bwHC site ${site.value}".asLeft[Option[Snapshot[MTBFile]]]
        )

      case Some(baseUrl) => {

        log.debug(s"Site: ${site.value}  URL: ${baseUrl}")

        request(baseUrl,"MTBFile:request")
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
              s"Error in Peer-to-peer Query response from bwHC Site ${site.value}: ${t.getMessage}".asLeft[Option[Snapshot[MTBFile]]]
          }
      
      }
    }

  }

}
