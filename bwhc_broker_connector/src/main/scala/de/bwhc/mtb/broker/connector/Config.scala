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
  def brokerBaseURL: URL
}


object Config
{
  
  //-----------------------------------------------------------------------------

  private case class Impl
  (
    siteId: String,
    baseURL: String
  )
  extends Config
  {
    override def brokerBaseURL =
      new URL(
        if (baseURL endsWith "/")
          baseURL
        else
          s"$baseURL/"
      )
  }

  
  private def parseXMLConfig(in: InputStream): Impl = {
    val xml = XML.load(in)

    val siteId =
      (xml \ "Site" \@ "id")

    val baseURL =
      (xml \ "Broker" \@ "baseURL")

    Impl(siteId,baseURL)
  }


  def getInstance: Config = {

    // Try reading JSON config by default
    Try {
      Option(getClass.getClassLoader.getResourceAsStream("bwhcConnectorConfig.xml")).get
    }
    .recoverWith {
      case t =>
        Try { Option(System.getProperty("bwhc.connector.configFile")).get }
          .map(new FileInputStream(_))
    }
    .flatMap(Using(_)(parseXMLConfig))
    .get

  }

}


