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
  Patient,
  Coding,
  ZPM
}

import de.bwhc.mtb.data.entry.views.{
  MolecularTherapyView,
  MTBFileView,
  TherapyRecommendationView
}


trait QueryOps
{


  def process(
    cmd: QueryOps.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,Query]]

  final def !(
    cmd: QueryOps.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ) = process(cmd)


  def get(
    id: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]]


  def resultSummaryOf(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[ResultSummary]]


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


  def mtbFileViewFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFileView]]


  def ngsSummariesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[NGSSummary]]]


  def therapyRecommendationsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[TherapyRecommendationSummary]]]
//  ): Future[Option[Iterable[TherapyRecommendationView]]]


  def molecularTherapiesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[MolecularTherapySummary]]]
//  ): Future[Option[Iterable[MolecularTherapyView]]]


  def savedQueriesOf(
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[SavedQueryInfo]]


  def retrieveMTBFileSnapshot(
    patId: Patient.Id,
    snpId: Option[Snapshot.Id],
    site: Option[ZPM]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Option[MTBFileView]]]


}


object QueryOps
{

  sealed abstract class Command
  object Command
  {

    final case class Submit
    (
      mode: Coding[Query.Mode.Value],
      parameters: Query.Parameters
    )
    extends Command
  
    final case class Update
    (
      id: Query.Id,
      mode: Coding[Query.Mode.Value],
      parameters: Query.Parameters,
//      filter: Option[Query.Filter]
    )
    extends Command


    final case class ApplyFilter
    (
      id: Query.Id,
      filter: Query.Filter
    )
    extends Command


    final case class ApplyFilters
    (
      id: Query.Id,
      patientFilter: Option[Query.PatientFilter],
      ngsSummaryFilter: Option[Query.NGSSummaryFilter],
      therapyRecommendationFilter: Option[Query.TherapyRecommendationFilter],
      molecularTherapyFilter: Option[Query.MolecularTherapyFilter]
    )
    extends Command


    final case class Reset
    (
      id: Query.Id,
    ) 
    extends Command


    final case class Save
    (
      id: Query.Id,
      name: String,
      description: Option[String]
    ) 
    extends Command
 
    final case class Reload
    (
      id: Query.Id,
    ) 
    extends Command

    final case class Delete
    (
      id: Query.Id,
    ) 
    extends Command


    implicit val formatSubmit =
      Json.format[Submit]
 
    implicit val formatUpdate =
      Json.format[Update]
 
    implicit val formatApplyFilter =
      Json.format[ApplyFilter]

    implicit val formatApplyFilters =
      Json.format[ApplyFilters]


//    implicit val formatSave =
//      Json.format[Save]
 
  }

}
