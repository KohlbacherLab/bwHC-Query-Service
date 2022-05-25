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
import play.api.libs.ws.JsonBodyReadables._
import play.api.libs.ws.JsonBodyWritables._
import play.api.libs.ws.DefaultBodyWritables._
import play.api.libs.ws.DefaultBodyReadables._

import play.api.libs.json.{Json,JsValue}

import cats.data.{Ior,IorNel,NonEmptyList}
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
//    val site    = config.localSite
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
//    val site    = config.localSite
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
//    val site    = config.localSite
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
