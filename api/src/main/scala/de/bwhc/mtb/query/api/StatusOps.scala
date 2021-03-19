package de.bwhc.mtb.query.api



import java.time.LocalDateTime

import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}

import cats.data.IorNel

import play.api.libs.json._

import de.bwhc.mtb.data.entry.dtos.
{ 
  MTBFile,
  ZPM
}


object PeerStatus extends Enumeration
{
  val Online, Offline = Value

  implicit val format = Json.formatEnum(this)
}

final case class PeerStatusReport
(
  dateTime: LocalDateTime = LocalDateTime.now,
  peerStatus: List[PeerStatusReport.Info]
)

object PeerStatusReport
{

  final case class Info
  (
    site: ZPM,
    status: PeerStatus.Value,
    details: String
  )

  object Info
  {
    implicit val format = Json.format[Info]
  }

  implicit val format = Json.format[PeerStatusReport]

}




trait StatusOps
{

  def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport]

}
