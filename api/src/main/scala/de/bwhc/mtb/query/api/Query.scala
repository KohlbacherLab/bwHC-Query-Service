package de.bwhc.mtb.query.api


import java.time.{Instant,YearMonth}

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

import de.bwhc.util.data.{Interval,ClosedInterval}
import de.bwhc.util.json.time._

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Gender,
  ICD10GM,
  ICDO3M,
  Patient,
  Medication,
  RECIST,
  Variant,
  SimpleVariant,
  CNV,
  Gene,
  ZPM,
  ECOG,
  Specimen,
  LevelOfEvidence,
  TherapyRecommendation,
  MolecularTherapy
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
  filters: Query.Filters,
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
        Local     -> "Lokal",
        Federated -> "Föderiert"
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
         Used        -> "Verabreicht",
         Recommended -> "Empfohlen"
      )

    implicit val system =
      Coding.System[DrugUsage.Value](ValSet[DrugUsage.Value].name)
  }


  case class MedicationWithUsage
  (
    medication: Coding[Medication.Code],
    usage: Set[Coding[DrugUsage.Value]]
  )

  implicit val formatMedicationWithUsage =
    Json.format[MedicationWithUsage]


  final case class SNVParameters
  (
    gene: Coding[Gene.HgncId],
    dnaChange: Option[SimpleVariant.DNAChange],
    aminoAcidChange: Option[SimpleVariant.AminoAcidChange],
  )

  final case class CNVParameters
  (
    genes: List[Coding[Gene.HgncId]],
    `type`: Option[CNV.Type.Value],
    copyNumber: Option[Interval[Int]]
  )


  final case class FusionParameters
  (
    fivePrimeGene: Option[Coding[Gene.HgncId]],
    threePrimeGene: Option[Coding[Gene.HgncId]],
  )


  final case class Parameters
  (
    diagnoses: Option[List[Coding[ICD10GM]]],
    tumorMorphology: Option[List[Coding[ICDO3M]]],
    mutatedGenes: Option[List[Coding[Gene.HgncId]]],
    simpleVariants: Option[List[SNVParameters]],
    copyNumberVariants: Option[List[CNVParameters]],
    dnaFusions: Option[List[FusionParameters]],
    rnaFusions: Option[List[FusionParameters]],
    medicationsWithUsage: Option[List[MedicationWithUsage]],
    responses: Option[List[Coding[RECIST.Value]]]
  )


  object Parameters
  {
    val empty = 
      Parameters(
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
  }


  implicit val formatSNVParameters    = Json.format[SNVParameters]
  implicit val formatCNVParameters    = Json.format[CNVParameters]
  implicit val formatFusionParameters = Json.format[FusionParameters]
  implicit val formatParameters       = Json.format[Parameters]


  final case class PatientFilter 
  (
    gender: Selection[Coding[Gender.Value]],
    ageRange: ClosedInterval[Int],
    vitalStatus: Selection[Coding[VitalStatus.Value]]
  )

  final case class NGSSummaryFilter 
  (
    specimenType: Selection[Coding[Specimen.Type.Value]],
    specimenLocalization: Selection[Coding[Specimen.Collection.Localization.Value]],
    tumorMutationalBurden: ClosedInterval[Int]
  )

  final case class TherapyRecommendationFilter 
  (
    priority: Selection[TherapyRecommendation.Priority.Value],
    levelOfEvidence: Selection[LevelOfEvidence.Grading.Value],
    medication: Selection[Coding[Medication.Code]]
  )

  final case class MolecularTherapyFilter 
  (
    status: Selection[Coding[MolecularTherapy.Status.Value]],
    recordingDate: ClosedInterval[YearMonth],
    medication: Selection[Coding[Medication.Code]],
    response: Selection[Coding[RECIST.Value]]
  )

  final case class Filters
  (
    patientFilter: PatientFilter,
    ngsSummaryFilter: NGSSummaryFilter,
    therapyRecommendationFilter: TherapyRecommendationFilter,
    molecularTherapyFilter: MolecularTherapyFilter
  )

  implicit val formatPatFilter   = Json.format[PatientFilter]
  implicit val formatNGSFilter   = Json.format[NGSSummaryFilter]
  implicit val formatThRecFilter = Json.format[TherapyRecommendationFilter]
  implicit val formatMolThFilter = Json.format[MolecularTherapyFilter]
  implicit val formatFilters     = Json.format[Filters]


  implicit val formatQuery = Json.format[Query]

}

