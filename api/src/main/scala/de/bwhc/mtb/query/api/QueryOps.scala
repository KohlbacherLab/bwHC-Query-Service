package de.bwhc.mtb.query.api


import java.time.{
  Instant,LocalDateTime
}

import scala.util.{
  Either, Try
}
import scala.concurrent.{
  Future, ExecutionContext
}

import cats.data.{
  Ior, IorNel, NonEmptyList
}

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  Patient
}


trait QueryOps
{


  def process(
    cmd: QueryOps.Command
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,Query]]

  def !(cmd: QueryOps.Command)(implicit ec: ExecutionContext) = process(cmd)


  def get(
    id: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]]


  def resultsOf(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]]



  def patientsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[PatientView]]]


  def mtbFileFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]]


/*
  def therapyRecommendationsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[TherapyRecommendation]]]


  def ngsSummariesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[NGSReportSummary]]]


  def molecularTherapiesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[MolecularTherapyView]]

*/

}


object QueryOps
{

  sealed abstract class Command
  object Command
  {

    final case class Submit
    (
      querier: Querier,
      mode: Query.Mode.Value,
      parameters: Query.Parameters
    ) extends Command
  
    final case class Update
    (
      id: Query.Id,
      mode: Query.Mode.Value,
      parameters: Query.Parameters,
      filter: Option[Query.Filter]
    ) extends Command
  
    final case class ApplyFilter
    (
      id: Query.Id,
      filter: Query.Filter
    ) extends Command
 
 
    implicit val formatSubmit =
      Json.format[Submit]
 
    implicit val formatUpdate =
      Json.format[Update]
 
    implicit val formatApplyFilter =
      Json.format[ApplyFilter]

  }

}
