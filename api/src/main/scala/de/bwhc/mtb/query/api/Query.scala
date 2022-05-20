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

import de.bwhc.util.data.{Interval,ClosedInterval}

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
/*
  ECOG,
  Specimen,
  LevelOfEvidence,
  TherapyRecommendation,
  MolecularTherapy
*/
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
    genes: Set[Coding[Gene.HgncId]],
    `type`: Option[CNV.Type.Value],
    copyNumber: Option[Interval[Int]]
//    copyNumber: Option[ClosedInterval[Int]]
  )


  final case class Parameters
  (
    diagnoses: Option[Set[Coding[ICD10GM]]],
    tumorMorphology: Option[Set[Coding[ICDO3M]]],
    mutatedGenes: Option[Set[Coding[Gene.HgncId]]],
    simpleVariants: Option[Set[SNVParameters]],
    copyNumberVariants: Option[Set[CNVParameters]],
    tumorMutationalBurden: Option[Interval[Double]],
//    tumorMutationalBurden: Option[Interval[Int]],
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
        None,
        None,
        None,
        None
      )
  }


  implicit val formatSNVParameters = Json.format[SNVParameters]
  implicit val formatCNVParameters = Json.format[CNVParameters]
  implicit val formatParameters    = Json.format[Parameters]


/*
  final case class PatientFilter 
  (
    gender: Set[Gender.Value],
    age: ClosedInterval[Int],
    vitalStatus: Set[VitalStatus.Value]
  )

  final case class NGSSummaryFilter 
  (
    specimenType: Set[Specimen.Type.Value],
    specimenLocalization: Set[Specimen.Collection.Localization.Value],
    tumorCellContent: ClosedInterval[Int]
  )

  final case class TherapyRecommendationFilter 
  (
    ecogStatus: Set[ECOG.Value],
    priority: Set[TherapyRecommendation.Priority.Value],
    levelOfEvidence: Set[LevelOfEvidence.Grading.Value],
    medication: Set[Coding[Medication.Code]]
  )

  final case class MolecularTherapyFilter 
  (
    status: Set[MolecularTherapy.Status.Value],
    medication: Set[Coding[Medication.Code]]
    response: Set[RECIST.Value]   
  )

  final case class Filters
  (
    patient: PatientFilter,
    ngsSummaries: NGSSummaryFilter,
    therapyRecommendation: TherapyRecommendationFilter,
    molecularTherapy: MolecularTherapyFilter
  )

  implicit val formatPatFilter   = Json.format[PatientFilter]
  implicit val formatNGSFilter   = Json.format[NGSFilter]
  implicit val formatThRecFilter = Json.format[TherapyRecommendationFilter]
  implicit val formatMolThFilter = Json.format[MolecularTherapyFilter]
  implicit val formatFilters     = Json.format[Filters]
*/

  final case class Filter
  (
    genders: Set[Gender.Value],
    ageRange: ClosedInterval[Int],
    vitalStatus: Set[VitalStatus.Value]
  )



  implicit val formatFilter = Json.format[Filter]

  implicit val formatQuery = Json.format[Query]

}

