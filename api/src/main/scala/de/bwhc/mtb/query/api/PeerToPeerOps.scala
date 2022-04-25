package de.bwhc.mtb.query.api


import java.time.Instant
import scala.concurrent.{ExecutionContext,Future}
import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  MTBFile,
  ZPM
}
import play.api.libs.json.Json


final case class PeerToPeerQuery
(
  id: Query.Id,
  origin: ZPM,
  querier: Querier,
  parameters: Query.Parameters,
  submittedAt: Instant = Instant.now
)

object PeerToPeerQuery
{
  implicit val format = Json.format[PeerToPeerQuery]
}


final case class PeerToPeerMTBFileRequest
(
  origin: ZPM,
  querier: Querier,
  patId: Patient.Id,
  snpId: Option[Snapshot.Id],
  submittedAt: Instant = Instant.now
)

object PeerToPeerMTBFileRequest
{
  implicit val format = Json.format[PeerToPeerMTBFileRequest]
}



trait PeerToPeerOps
{

  //---------------------------------------------------------------------------
  // Peer-to-peer Operations
  //---------------------------------------------------------------------------

  def resultsOf(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]]


  def process(
    req: PeerToPeerMTBFileRequest
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]]


}

