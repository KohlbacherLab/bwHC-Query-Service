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

  implicit def ordering[T]: Ordering[Snapshot[T]] =
    Ordering.by((snp: Snapshot[T]) => snp.timestamp)

  implicit val formatId = Json.valueFormat[Id]

  implicit def format[T: Format] = Json.format[Snapshot[T]]
}
