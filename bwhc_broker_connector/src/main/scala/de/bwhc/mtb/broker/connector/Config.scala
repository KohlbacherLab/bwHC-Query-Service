package de.bwhc.mtb.broker.connector



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
  def siteId: String
  def baseURL: URL
  def timeout: Option[Int]
  def updatePeriod: Option[Long]
}


object Config
{

  final case class Proxy
  (
    host: String,
    port: Option[Int]
  )
  
  //-----------------------------------------------------------------------------

  private case class Impl
  (
    siteId: String,
    url: String,
    timeout: Option[Int],
    updatePeriod: Option[Long]
  )
  extends Config
  {
    override def baseURL =
      new URL(
        if (url endsWith "/")
          url
        else
          s"$url/"
      )

  }

  
  private def parseXMLConfig(in: InputStream): Impl = {

    val xml = XML.load(in)

    Impl(
      (xml \ "Site" \@ "id"),
      (xml \ "Broker" \@ "baseURL"),
      Option(xml \ "Timeout" \@ "seconds").map(_.toInt),
      Option((xml \ "Update" \@ "period")).map(_.toLong)
    )
  }


  def getInstance: Config = {

    // Try reading config from classpath by default
    Try {
      Option(getClass.getClassLoader.getResourceAsStream("bwhcConnectorConfig.xml")).get
    }
    // else use system property for configFile path
    .recoverWith {
      case t =>
        Try { Option(System.getProperty("bwhc.connector.configFile")).get }
          .map(new FileInputStream(_))
    }
    .flatMap(Using(_)(parseXMLConfig))
    // else use system properties for siteId and baseUrl to instantiate Config
    .recoverWith {
      case t => 
        Try {
          for {
            siteId    <- Option(System.getProperty("bwhc.connector.config.siteId"))
            baseUrl   <- Option(System.getProperty("bwhc.connector.config.baseUrl"))
            timeout   =  Option(System.getProperty("bwhc.connector.config.timeout")).map(_.toInt)
            period    =  Option(System.getProperty("bwhc.connector.config.update.period")).map(_.toLong)
          } yield
            Impl(
              siteId,
              baseUrl,
              timeout,
              period
            )
        }
        .map(_.get)
    }
    .get

  }

}


