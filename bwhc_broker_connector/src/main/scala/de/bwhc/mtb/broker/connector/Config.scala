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
import de.bwhc.mtb.data.entry.dtos.ZPM


trait Config
{  
  def siteId: String
  def baseURL: URL
  def updatePeriod: Option[Long]
}


object Config
{
  
  //-----------------------------------------------------------------------------

  private case class Impl
  (
    siteId: String,
    url: String,
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
            siteId  <- Option(System.getProperty("bwhc.connector.config.siteId"))
            baseUrl <- Option(System.getProperty("bwhc.connector.config.baseUrl"))
            period  =  Option(System.getProperty("bwhc.connector.config.update.period"))
                         .map(_.toLong)
          } yield Impl(siteId,baseUrl,period)
        }
        .map(_.get)
    }
    .get

  }

}


