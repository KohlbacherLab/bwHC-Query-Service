package de.bwhc.mtb.query.api


import java.time.LocalDate


import de.bwhc.mtb.data.entry.dtos.{
  Specimen,
  SomaticNGSReport,
  ValueSet,
}

import de.bwhc.mtb.data.entry.views.{
  ICD10Display,
  NotAvailable,
  TumorCellContentDisplay
}


trait Mappings
{

  import de.bwhc.mtb.data.entry.views.mappings._

  import de.bwhc.mtb.data.entry.dtos.ValueSets._


  implicit val specimenAndReportToSummary: ((SomaticNGSReport,Option[Specimen])) => NGSSummary = {

    case (ngs,specimen) =>

      NGSSummary(
        ngs.patient,
        ngs.specimen,
        specimen.map(_.icd10.mapTo[ICD10Display]) toRight NotAvailable,
        specimen.flatMap(_.`type`).flatMap(ValueSet[Specimen.Type.Value].displayOf) toRight NotAvailable,
        ngs.sequencingType,
        ngs.tumorCellContent.mapTo[TumorCellContentDisplay]
      )

  }


  implicit val specimensAndReportsToSummaries: ((List[SomaticNGSReport],List[Specimen])) => List[NGSSummary] = {
    case (reports,specimens) =>
      reports.map(
        ngs =>
          (ngs -> specimens.find(_.id == ngs.specimen)).mapTo[NGSSummary]
      )
     
  }


}
object Mappings extends Mappings

