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


final case class PeerStatusInfo
(
  site: ZPM,
  status: PeerStatus.Value
)

object PeerStatusInfo
{
  
  implicit val format = Json.format[PeerStatusInfo]
}


trait StatusOps
{

  def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[List[PeerStatusInfo]]

}
