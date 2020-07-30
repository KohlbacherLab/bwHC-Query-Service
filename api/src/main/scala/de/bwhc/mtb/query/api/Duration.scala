package de.bwhc.mtb.query.api



import java.time.temporal.ChronoUnit

import play.api.libs.json._


final case class Duration
(
  value: Double,
  unit: ChronoUnit
)
{

  def +(other: Duration): Duration = {
    Duration(value + other.value, unit)
  }

  def /(div: Double): Duration = {
    Duration(value/div,unit)
  }

}

object Duration
{

  implicit val formatChronoUnit =
    Format[ChronoUnit](
      Reads(js => js.validate[String].map(ChronoUnit.valueOf)),
      Writes(cu => JsString(cu.toString.toUpperCase))
    )

  implicit val formatDuration: Format[Duration] =
    Json.format[Duration]
}


