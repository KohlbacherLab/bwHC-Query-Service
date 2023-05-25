package de.bwhc.mtb.query.api


import java.time.LocalDate
import play.api.libs.json.Json
import de.bwhc.mtb.dtos.{
  Patient,
  TherapyRecommendation,
  TherapyId,
  Dosage,
  LevelOfEvidence
}
import de.bwhc.mtb.views.{
  Or,
  NotAvailable,
  NoValue,
  ICD10Display,
  MedicationDisplay,
  SupportingVariantDisplay,
  ResponseDisplay,
  PeriodDisplay
}


final case class MolecularTherapySummary
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: NotAvailable Or ICD10Display,
  status: String,
  recordedOn: LocalDate,
  recommendation: TherapyRecommendation.Id,
  recommendationPriority: NotAvailable Or TherapyRecommendation.Priority.Value,
  recommendationLoE: NotAvailable Or LevelOfEvidence.Grading.Value,
  period: NotAvailable Or PeriodDisplay[LocalDate],
  notDoneReason: NoValue Or String,
  medication: NoValue Or MedicationDisplay,
  medicationClasses: NoValue Or MedicationDisplay,
  supportingVariants: List[SupportingVariantDisplay],
  reasonStopped: NoValue Or String,
  dosage: NotAvailable Or Dosage.Value,
  note: String,
  response: NotAvailable Or ResponseDisplay,
  progressionDate: NoValue Or LocalDate
)

object MolecularTherapySummary
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[MolecularTherapySummary]
}
