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
  def localSite: ZPM

  def brokerBaseURL: URL
}


object Config
{
  
  //-----------------------------------------------------------------------------

  private case class Impl
  (
    localSite: ZPM,
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

    val site =
      Option(System.getProperty("bwhc.zpm.site"))
        .getOrElse((xml \ "ZPM" \@ "site"))

    val baseURL =
      (xml \ "Broker" \@ "baseURL")

    Impl(ZPM(site),baseURL)
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


