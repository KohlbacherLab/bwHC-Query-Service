package de.bwhc.mtb.query.connector



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
//import play.api.libs.ws.StandaloneWSResponse
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


/*
  private def gather[T](
    responses: List[Future[StandaloneWSResponse]]]
  )(
    implicit ec: ExecutionContext
  ): Future[Ior[]]
*/

  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = {

    import PeerStatus._

    val requests =
      for {
        (site,baseUrl) <- config.peerBaseURLs

        _ = log.debug(s"Site: ${site.value}  URL: ${baseUrl.toString}")

        req =
          wsclient.url(baseUrl.toString + "status")
            .withRequestTimeout(timeout)
            .get
            .map(
              response =>
                if (response.status == OK)
                  PeerStatusReport.Info(site,Online,"-")
                else
                  PeerStatusReport.Info(site,Offline,response.body[String])
            )
            .recover {
              case t => PeerStatusReport.Info(site,Offline,t.getMessage)
            }

      } yield req

   
     Future.foldLeft(requests)(List.empty[PeerStatusReport.Info])(_ :+ _)
       .map(st => PeerStatusReport(peerStatus = st)) 

  }



  override def requestQCReports(
    origin: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

    log.debug(s"Requesting LocalQCReports from peers for Querier ${querier.value}")

    val requests =
      for {
        (zpm,baseUrl) <- config.peerBaseURLs

        _ = log.debug(s"Site: ${zpm.value}  URL: ${baseUrl.toString}")

        req =
          wsclient.url(baseUrl.toString + "LocalQCReport")
            .withRequestTimeout(timeout)
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
                s"Error in LocalQCReport response from bwHC Site ${zpm.value}: ${t.getMessage}".leftIor[LocalQCReport].toIorNel
            }
            .map(_.map(List(_)))
      } yield req


    Future.foldLeft(requests)(List.empty[LocalQCReport].rightIor[String].toIorNel)(_ combine _)

  }


  override def execute(
    q: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

    log.debug(s"Executing Peer-to-Peer Query ${q}") //TODO: Query to JSON

    val requests =
      for {
        (zpm,baseUrl) <- config.peerBaseURLs

        _ = log.debug(s"Site: ${zpm.value}  URL: ${baseUrl.toString}")

        req =
          wsclient.url(baseUrl.toString + "query")
            .withRequestTimeout(timeout)
            .post(Json.toJson(q))
            .map(_.body[JsValue].as[SearchSet[Snapshot[MTBFile]]]) //TODO: handle validation errors
            .map(_.entries) 
            .map(_.rightIor[String].toIorNel)
            .recover {
              case t => 
                s"Error in Peer-to-peer Query response from bwHC Site ${zpm.value}: ${t.getMessage}".leftIor[List[Snapshot[MTBFile]]].toIorNel
            }
      } yield req

    Future.foldLeft(requests)(List.empty[Snapshot[MTBFile]].rightIor[String].toIorNel)(_ combine _)

  }

  override def execute(
    site: ZPM,
    mfreq: PeerToPeerMTBFileRequest
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

        log.debug(s"Site: ${site.value}  URL: ${baseUrl.toString}")

        wsclient.url(baseUrl.toString + "MTBFile:request")
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
              s"Error in Peer-to-peer Query response from bwHC Site ${site.value}: ${t.getMessage}".asLeft[Option[Snapshot[MTBFile]]]
          }
      
      }
    }

  }

}
