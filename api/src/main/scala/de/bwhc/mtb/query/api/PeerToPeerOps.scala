package de.bwhc.mtb.query.api


import java.time.Instant
import scala.concurrent.{ExecutionContext,Future}
import de.bwhc.mtb.dtos.{
  Patient,
  MTBFile,
  ZPM
}
import play.api.libs.json.{
  Json,
  Format,
  Writes,
  Reads
}


final case class MTBFileParameters
(
  patId: Patient.Id,
  snpId: Option[Snapshot.Id]
)

object MTBFileParameters
{
  implicit val format =
    Json.format[MTBFileParameters]
}


final case class PeerToPeerRequest[+T]
(
  origin: ZPM,
  querier: Querier,
  body: T,
  submittedAt: Instant = Instant.now
)

object PeerToPeerRequest
{

  def apply(
    origin: ZPM,
    querier: Querier,
  ): PeerToPeerRequest[Map[String,String]] =
    PeerToPeerRequest(
      origin,
      querier,
      Map.empty[String,String]
    )


  implicit def writes[T: Writes] =
    Json.writes[PeerToPeerRequest[T]]

  implicit def reads[T: Reads] =
    Json.reads[PeerToPeerRequest[T]]

}



trait PeerToPeerOps
{

  //---------------------------------------------------------------------------
  // Peer-to-peer Operations
  //---------------------------------------------------------------------------

  def resultsOf(
    query: PeerToPeerRequest[Query.Parameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]]


  def process(
    req: PeerToPeerRequest[MTBFileParameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]]


}

