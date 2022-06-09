package de.bwhc.mtb.query.api


import play.api.libs.json.Json
import de.bwhc.mtb.data.entry.dtos.Patient
import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICD10GM,
  ICDO3M,
  Medication,
  RECIST,
  Variant
}
import de.bwhc.mtb.data.entry.views.{
  ICD10Display,
  ICDO3MDisplay,
  MedicationDisplay,
  SupportingVariantDisplay,
  ResponseDisplay
}



final case class QueryMatchSummary
(
  id: Patient.Id,
  rsv: Option[Double],   // Retrieval Status Value
  diagnoses: Option[Set[Coding[ICD10GM]]],
  tumorMorphology: Option[Set[Coding[ICDO3M]]],
  variants: Option[Set[Variant]],
  medications: Option[Set[Coding[Medication.Code]]],
  responses: Option[Set[Coding[RECIST.Value]]]
)


final case class QueryMatchSummaryView
(
  id: Patient.Id,
  rsv: Option[Double],   // Retrieval Status Value
  diagnoses: Option[Set[ICD10Display]],
  tumorMorphology: Option[Set[ICDO3MDisplay]],
  variants: Option[Set[SupportingVariantDisplay]],
  medications: Option[Set[MedicationDisplay]],
  responses: Option[Set[ResponseDisplay]]
)


object QueryMatchSummaryView
{

  implicit val format =
    Json.writes[QueryMatchSummaryView]
}
