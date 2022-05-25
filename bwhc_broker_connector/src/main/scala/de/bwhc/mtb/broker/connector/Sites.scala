package de.bwhc.mtb.broker.connector


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
    name: String
  )

  implicit val formatEntry = Json.writes[Entry]
  implicit val format      = Json.writes[Sites]

}

