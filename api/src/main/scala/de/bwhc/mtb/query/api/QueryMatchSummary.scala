package de.bwhc.mtb.query.api


import play.api.libs.json.Json
import de.bwhc.mtb.dtos.Patient
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  ICDO3M,
  Medication,
  RECIST,
  Variant
}
import de.bwhc.mtb.views.{
  ICD10Display,
  ICDO3MDisplay,
  MedicationDisplay,
  SupportingVariantDisplay,
  ResponseDisplay
}


/*
final case class QueryMatchSummary
(
  id: Patient.Id,
  rsv: Option[Double],   // Retrieval Status Value
  diagnoses: Option[List[Coding[ICD10GM]]],
  tumorMorphology: Option[List[Coding[ICDO3M]]],
  variants: Option[List[Variant]],
  medications: Option[List[Coding[Medication.Code]]],
  responses: Option[List[Coding[RECIST.Value]]]
)
*/

final case class QueryMatchSummary
(
  id: Patient.Id,
  rsv: Option[Double],   // Retrieval Status Value
  diagnoses: Option[List[ICD10Display]],
  tumorMorphology: Option[List[ICDO3MDisplay]],
  variants: Option[List[SupportingVariantDisplay]],
  medications: Option[List[MedicationDisplay]],
  responses: Option[List[ResponseDisplay]]
)


object QueryMatchSummary
{

  implicit val format =
    Json.writes[QueryMatchSummary]
}
