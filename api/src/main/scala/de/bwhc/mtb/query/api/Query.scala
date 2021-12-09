package de.bwhc.mtb.query.api


import java.time.Instant

import scala.util.Either

import scala.concurrent.{
  Future,
  ExecutionContext
}

import cats.data.{
  Ior,
  IorNel,
  NonEmptyList
}

import play.api.libs.json.Json

import de.bwhc.util.data.ClosedInterval

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Gender,
  ICD10GM,
  Patient,
  Medication,
  RECIST,
  Variant,
  Gene,
  ZPM
}


final case class Querier(value: String) extends AnyVal

object Querier
{
  implicit val format = Json.valueFormat[Querier]
}


final case class Query
(
  id: Query.Id,
  querier: Querier,
  submittedAt: Instant,
//  mode: Coding[Query.Mode.Value],
  mode: Query.Mode.Value,
  parameters: Query.Parameters,
  filter: Query.Filter,
  zpms: Set[ZPM],
  lastUpdate: Instant
)


object Query
{

  final case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  object Mode extends Enumeration
  {
    val Local     = Value("lokal")
    val Federated = Value("föderiert")
//    val Local     = Value("local")
//    val Federated = Value("federated")

    implicit val format = Json.formatEnum(this)
  }


  object DrugUsage  extends Enumeration
  {
//    val Used        = Value("used")
//    val Recommended = Value("recommended")
    val Used        = Value("verabreicht")
    val Recommended = Value("empfohlen")

    implicit val format  = Json.formatEnum(this)
  }

/*
  case class MedicationWithUsage
  (
    code: Medication.Code,
    usage: DrugUsage.Value
  )
*/

  case class MedicationWithUsage
  (
    medication: Coding[Medication.Code],
    usage: DrugUsage.Value
  )

  implicit val formatMedicationWithUsage =
    Json.format[MedicationWithUsage]

 /*
  final case class Parameters
  (
    diagnoses: Option[Set[ICD10GM]],
    mutatedGenes: Option[Set[Gene.HgncId]],
    medicationsWithUsage: Option[Set[MedicationWithUsage]],
    responses: Option[Set[RECIST.Value]]
  )
*/

  final case class Parameters
  (
    diagnoses: Option[Set[Coding[ICD10GM]]],
    mutatedGenes: Option[Set[Coding[Gene.HgncId]]],
    medicationsWithUsage: Option[Set[MedicationWithUsage]],
    responses: Option[Set[Coding[RECIST.Value]]]
  )


  object Parameters
  {
    val empty = 
      Parameters(
        None,
        None,
        None,
        None
      )
  }


  implicit val formatParameters =
    Json.format[Parameters]


  final case class Filter
  (
    genders: Set[Gender.Value],
    ageRange: ClosedInterval[Int],
    vitalStatus: Set[VitalStatus.Value]
  )

  implicit val formatFilter =
    Json.format[Filter]

  implicit val formatQuery =
    Json.format[Query]

}


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

