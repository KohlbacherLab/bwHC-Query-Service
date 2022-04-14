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

import play.api.libs.json.{Json,Format,Reads,Writes,JsObject}

import de.bwhc.util.data.ClosedInterval

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Gender,
  ICD10GM,
  ICDO3M,
  Patient,
  Medication,
  RECIST,
  Variant,
  Gene,
  ZPM,
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
  mode: Coding[Query.Mode.Value],
  parameters: Query.Parameters,
  filter: Query.Filter,
  zpms: Set[ZPM],
  lastUpdate: Instant
)


object Query
{

  final case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  import de.bwhc.mtb.data.entry.dtos.{ValueSet => ValSet}
  import ValSet.Concept

  object Mode extends Enumeration
  {
    val Local     = Value("local")
    val Federated = Value("federated")

    implicit val format = Json.formatEnum(this)

    implicit val valueSetDE =
      ValSet[Mode.Value](
        "Query-Mode",
        List(
          Concept(Local,    "Lokal"),
          Concept(Federated,"Föderiert")
        )
      )

    implicit val system =
      Coding.System[Mode.Value](ValSet[Mode.Value].name)

  }


  object DrugUsage  extends Enumeration
  {
    val Used        = Value("used")
    val Recommended = Value("recommended")

    implicit val format  = Json.formatEnum(this)

    implicit val valueSetDE =
      ValSet[DrugUsage.Value](
        "Drug-Usage",
        List(
          Concept(Used,       "Verabreicht"),
          Concept(Recommended,"Empfohlen")
        )
      )

    implicit val system =
      Coding.System[DrugUsage.Value](ValSet[DrugUsage.Value].name)
  }


  case class MedicationWithUsage
  (
    medication: Coding[Medication.Code],
//    usage: Coding[DrugUsage.Value]
    usage: Set[Coding[DrugUsage.Value]]
  )

  implicit val formatMedicationWithUsage =
    Json.format[MedicationWithUsage]


  final case class Parameters
  (
    diagnoses: Option[Set[Coding[ICD10GM]]],
    tumorMorphology: Option[Set[Coding[ICDO3M]]],
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
    vitalStatus: Set[VitalStatus.Value],
//    priorities: Set[Priority.Value],
//    levelsOfEvidence: Set[LevelOfEvidence.Grading],
//    responses: Set[RECIST.Value],
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
