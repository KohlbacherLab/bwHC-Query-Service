package de.bwhc.mtb.query.api



import java.time.LocalDate

import play.api.libs.json.Json
import de.bwhc.util.json._

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  Specimen,
  SomaticNGSReport
}

import de.bwhc.mtb.data.entry.views.{
  ICD10Display,
  Or,
  NotAvailable,
  TumorCellContentDisplay
}


final case class NGSSummary
(
  patient: Patient.Id,
  specimen: Specimen.Id,
  tumorEntity: NotAvailable Or ICD10Display,
  specimenType: NotAvailable Or String,
  sequencingType: SomaticNGSReport.SequencingType,
  tumorCellContent: TumorCellContentDisplay,
  //TODO: add Variant summary?
)

object NGSSummary
{
  implicit val format = Json.writes[NGSSummary]
}
