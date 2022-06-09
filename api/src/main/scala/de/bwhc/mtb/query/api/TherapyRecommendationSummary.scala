package de.bwhc.mtb.query.api


import play.api.libs.json.Json
import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  TherapyRecommendation,
  LevelOfEvidence
}
import de.bwhc.mtb.data.entry.views.{
  Or,
  NotAvailable,
  ICD10Display,
  MedicationDisplay,
  ECOGDisplay,
  SupportingVariantDisplay
}


final case class TherapyRecommendationSummary
(
  id: TherapyRecommendation.Id,
  patient: Patient.Id,
  icd10: NotAvailable Or ICD10Display,
  ecogStatus: NotAvailable Or ECOGDisplay,
  medication: NotAvailable Or MedicationDisplay,
  medicationClasses: NotAvailable Or MedicationDisplay,
  priority: NotAvailable Or TherapyRecommendation.Priority.Value,
//  levelOfEvidence: NotAvailable Or LevelOfEvidenceDisplay,
  levelOfEvidence: NotAvailable Or LevelOfEvidence.Grading.Value,
  supportingVariants: List[SupportingVariantDisplay]
)

object TherapyRecommendationSummary
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[TherapyRecommendationSummary]
}

