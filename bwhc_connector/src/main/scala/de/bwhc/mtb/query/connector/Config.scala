package de.bwhc.mtb.query.connector



import java.io.{
  FileInputStream,
  InputStream
}
import java.net.{URI,URL}

import scala.util.{Try,Using}

import scala.xml._

import play.api.libs.json.{
  Json,
  JsObject,
  JsValue
}

import de.bwhc.mtb.dtos.ZPM

trait Config
{  
  def timeout: Option[Int]
  def peerBaseURLs: Map[ZPM,URL]
}


object Config
{
  
  //-----------------------------------------------------------------------------

  private case class Impl
  (
    timeout: Option[Int],
    sites: List[SiteWithBaseURL]
  )
  extends Config
  {
    val peerBaseURLs =
      sites.map {
        case SiteWithBaseURL(site,url) =>

          val baseURL =
            if (url endsWith "/") url else s"$url/"

          (ZPM(site),new URL(baseURL))
      }
      .toMap
  }

  private case class SiteWithBaseURL(site: String, baseURL: String)

  private object Impl
  {
    implicit val formatSiteWithBaseURL =
      Json.format[SiteWithBaseURL]

    implicit val formatConfigImpl =
      Json.format[Impl]
  }
  //-----------------------------------------------------------------------------

  
  private def parseXMLConfig(in: InputStream): Impl = {
    val xml = XML.load(in)

    val timeout =
      Option(xml \ "Timeout" \@ "seconds").map(_.toInt)

    val sites =
      for {
        zpm <- (xml \ "ZPM")
        site = (zpm \@ "site")
        url  = (zpm \@ "baseURL")
      } yield SiteWithBaseURL(site,url)

    Impl(timeout,sites.toList)
  }


  def getInstance: Config = {

    // Try reading JSON config by default
    Try {
      Option(getClass.getClassLoader.getResourceAsStream("bwhcConnectorConfig.json")).get
    }
    .recoverWith {
      case t =>
        Try { Option(System.getProperty("bwhc.connector.configFile")).get }
          .map(new FileInputStream(_))
    }
    .flatMap(Using(_)(Json.parse(_).as[Config.Impl]))
    .recoverWith {
      // Fall back to XML config
      case t =>
        Try {
          Option(getClass.getClassLoader.getResourceAsStream("bwhcConnectorConfig.xml")).get
        }
        .recoverWith {
          case t =>
            Try { Option(System.getProperty("bwhc.connector.configFile")).get }
              .map(new FileInputStream(_))
        }
        .flatMap(Using(_)(parseXMLConfig))
    }
    .get

  }

}


