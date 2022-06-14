package de.bwhc.mtb.broker.connector


import java.net.URI
import play.api.libs.json.Json


final case class Sites
(
  sites: List[Sites.Entry]
)

object Sites
{

  final case class Entry
  (
    id: String,
    name: String,
//    baseUri: URI
  )

  implicit val formatEntry = Json.format[Entry]
  implicit val format      = Json.format[Sites]

}

