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
  MTBFile,
  ZPM
}
import de.bwhc.mtb.query.api.{
  Querier,
  Query,
  PeerStatus,
  PeerStatusReport,
  PeerToPeerQuery,
  PeerToPeerMTBFileRequest,
  LocalQCReport,
  Snapshot
}

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

  private val BWHC_SITE_ORIGIN  = "bwhc-site-origin" 
  private val BWHC_QUERY_USERID = "bwhc-query-userid"

  private val OK = 200


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

    scatterGather("/bwhc/peer2peer/api/status")(
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
    origin: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

    log.debug(
      s"Requesting LocalQCReports from peers for Querier ${querier.value}"
    )

    scatterGather("bwhc/peer2peer/api/LocalQCReport")(
      (site,request) =>
        request
          .post(
            Map(
              BWHC_SITE_ORIGIN  -> origin.value,
              BWHC_QUERY_USERID -> querier.value
            )
          )
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


  override def execute(
    q: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

    log.debug(s"Executing Peer-to-Peer Query ${q}") //TODO: Query to JSON

    scatterGather("bwhc/peer2peer/api/query")(
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


  override def execute(
    site: ZPM,
    mfreq: PeerToPeerMTBFileRequest
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

            request("/bwhc/peer2peer/api/MTBFile:request",Some(virtualHost))
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
