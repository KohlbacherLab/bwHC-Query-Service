package de.bwhc.mtb.query.api


import java.time.Instant

import play.api.libs.json.{
  Format,
  Json
}


final case class Snapshot[T]
(
  id: Snapshot.Id,
  timestamp: Instant,
  data: T
)


object Snapshot
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit def format[T: Format] = Json.format[Snapshot[T]]
}
