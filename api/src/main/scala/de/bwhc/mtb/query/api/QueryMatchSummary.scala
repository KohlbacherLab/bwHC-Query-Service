package de.bwhc.mtb.query.api



import de.bwhc.mtb.data.entry.dtos.Patient
import de.bwhc.mtb.data.entry.views.{
  ICD10Display,
  ICDO3MDisplay,
  MedicationDisplay,
  TMBDisplay,
  SupportingVariantDisplay,
  ResponseDisplay
}


final case class QueryMatchSummary
(
  id: Patient.Id,
//  rsv: Option[Double],   // Retrieval Status Value
  diagnoses: Option[Set[ICD10Display]],
  tumorMorphology: Option[Set[ICDO3MDisplay]],
  mutations: Option[Set[SupportingVariantDisplay]],
  tumorMutationalBurden: Option[TMBDisplay],
  medications: Option[Set[MedicationDisplay]],
  responses: Option[Set[ResponseDisplay]]
)

