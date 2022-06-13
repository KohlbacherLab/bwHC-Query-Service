package de.bwhc.mtb.broker.connector



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

  private val timeout = 20 seconds

  private val BWHC_SITE_ORIGIN  = "bwhc-site-origin" 
  private val BWHC_QUERY_USERID = "bwhc-query-userid"

  private val OK = 200


  private def peers(
    implicit ec: ExecutionContext
  ): Future[List[Sites.Entry]] = { 

    log.debug(s"Requesting bwHC peer info")

    val site    = config.siteId
    val baseUrl = config.brokerBaseURL

    log.debug(s"Broker URL: ${baseUrl.toString}")

    wsclient.url(s"$baseUrl/sites")
      .withRequestTimeout(timeout)
      .addHttpHeaders(BWHC_SITE_ORIGIN -> site)
      .get
      .map(_.body[JsValue].as[Sites])
      .map(_.sites.filterNot(_.id == site))

  }



  private def scatter[T](
    rawUri: String
  )(
    f: (String,WSRequest) => T
  )(
    implicit ec: ExecutionContext
  ): Future[List[T]] = {
   
    val site    = config.siteId
    val baseUrl = config.brokerBaseURL

    val uri =
      if (rawUri startsWith "/") rawUri
      else "/" + rawUri

    for {
      sites <- peers

      requests =
        sites.map {
          case Sites.Entry(_,name,baseUri) =>
            f(
             name,
             wsclient.url(s"${baseUrl}${baseUri}$uri")
               .withRequestTimeout(timeout)
               .addHttpHeaders(BWHC_SITE_ORIGIN -> site)
            )
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


  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = {

    import PeerStatus._

    for {
      results <-
        scatter("/bwhc/peer2peer/api/status")(
          (site,request) =>
            request
              .addHttpHeaders(BWHC_SITE_ORIGIN -> site)
              .get
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
        )
      
      report <-
        Future.foldLeft(results)(List.empty[PeerStatusReport.Info])(_ :+ _)
          .map(st => PeerStatusReport(peerStatus = st))

    } yield report
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

    for {
      results <-
        scatter("bwhc/peer2peer/api/LocalQCReport")(
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
        )

      qcReports <-
        Future.foldLeft(results)(List.empty[LocalQCReport].rightIor[String].toIorNel)(_ combine _)

    } yield qcReports

  }


  override def execute(
    q: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

    log.debug(s"Executing Peer-to-Peer Query ${q}") //TODO: Query to JSON

    for {
      results <-
        scatter("bwhc/peer2peer/api/query")(
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
        )

      qcReports <-
        Future.foldLeft(results)(List.empty[Snapshot[MTBFile]].rightIor[String].toIorNel)(_ combine _)

    } yield qcReports

  }

  override def execute(
    site: ZPM,
    mfreq: PeerToPeerMTBFileRequest
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,Option[Snapshot[MTBFile]]]] = {

    log.debug(s"Executing MTBFile Snapshot Request ${mfreq}") //TODO: Query to JSON

    for {
      peerList <- peers

      result <-
        peerList.find(_.name == site.value) match {

          case None =>
            Future.successful(
              s"Invalid site name ${site.value}; should be one of [${peerList.map(_.name).mkString(",")}]".asLeft[Option[Snapshot[MTBFile]]]
            )

          case Some(Sites.Entry(id,name,baseUri)) => {

            log.debug(s"Site: $name")

            wsclient.url(s"${config.brokerBaseURL}${baseUri}bwhc/peer2peer/api/MTBFile:request")
              .withRequestTimeout(timeout)
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
                  s"Error in MTBFile retrieal query from site $name: ${t.getMessage}".asLeft[Option[Snapshot[MTBFile]]]
              }
      
          }
        }

    } yield result

  }

}

/*
class BwHCConnectorImpl
(
  private val wsclient: StandaloneWSClient,
  private val config: Config
)
extends BwHCConnector
with Logging
{

  private val timeout = 20 seconds

  private val BWHC_ORIGIN       = "Bwhc-Origin" 
  private val BWHC_RECIPIENT    = "Bwhc-Recipient" 

  private val BWHC_SITE_ORIGIN  = "bwhc-site-origin" 
  private val BWHC_QUERY_USERID = "bwhc-query-userid"

  private val ALL = "*"

  private val OK = 200


  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = {

    import PeerStatus._

    log.debug(s"Requesting bwHC node connection status report")

    val site    = config.siteId
    val baseUrl = config.brokerBaseURL

    log.debug(s"Broker URL: ${baseUrl.toString}")

    wsclient.url(baseUrl.toString + "statusReport")
      .withRequestTimeout(timeout)
      .addHttpHeaders(BWHC_SITE_ORIGIN -> site)
      .get
      .map(_.body[JsValue].as[PeerStatusReport])

  }


  override def requestQCReports(
    origin: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

    log.debug(s"Requesting LocalQCReports for Querier ${querier.value}")

    val site    = config.siteId
    val baseUrl = config.brokerBaseURL

    log.debug(s"Broker URL: ${baseUrl.toString}")

    wsclient.url(baseUrl.toString + "bwhc/peer2peer/api/LocalQCReport")
      .withRequestTimeout(timeout)
      .addHttpHeaders(
        BWHC_ORIGIN    -> site,
        BWHC_RECIPIENT -> ALL
      )
      .post(
        Map(
          BWHC_SITE_ORIGIN  -> origin.value,
          BWHC_QUERY_USERID -> querier.value
        )
      )
      .map {
        resp => 
          val js = resp.body[JsValue]

          val reports = (js \ "results").asOpt[List[LocalQCReport]]
          val errors  = (js \ "errors").asOpt[NonEmptyList[String]]

          Ior.fromOptions(errors,reports).get
      }
      .recover {
        case t => 
          s"Error in LocalQCReport request: ${t.getMessage}".leftIor[List[LocalQCReport]].toIorNel
      }

  }


  override def execute(
    q: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

    val jsQuery = Json.toJson(q)

    log.debug(s"Executing Peer-to-Peer Query ${Json.prettyPrint(jsQuery)}")

    val site    = config.siteId
    val baseUrl = config.brokerBaseURL

    log.debug(s"Broker URL: ${baseUrl.toString}")


    wsclient.url(baseUrl.toString + "bwhc/peer2peer/api/query")
      .withRequestTimeout(timeout)
      .addHttpHeaders(
        BWHC_ORIGIN    -> site,
        BWHC_RECIPIENT -> ALL
      )
      .post(jsQuery)
      .map {
        resp => 
          val js = resp.body[JsValue]

          val results = (js \ "results").asOpt[List[SearchSet[Snapshot[MTBFile]]]]
          val errors  = (js \ "errors").asOpt[NonEmptyList[String]]

          Ior.fromOptions(errors,results.map(_.map(_.entries).flatten)).get
      }
      .recover {
        case t => 
          s"Error in Peer-to-peer request: ${t.getMessage}".leftIor[List[Snapshot[MTBFile]]].toIorNel
      }

  }

}
*/
