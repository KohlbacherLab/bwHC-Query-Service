package de.bwhc.mtb.query.api


import java.time.LocalDate
import java.time.format.DateTimeFormatter


import de.bwhc.mtb.data.entry.dtos.{
  Specimen,
  SomaticNGSReport,
  Patient,
  Diagnosis,
  Gender,
  ValueSet,
}

import de.bwhc.mtb.data.entry.views.{
  DiagnosisView,
  ICD10Display,
  NotAvailable,
  TumorCellContentDisplay
}


trait Mappings
{

  import de.bwhc.mtb.data.entry.views.mappings._

  import de.bwhc.mtb.data.entry.dtos.ValueSets._

  import java.time.temporal.ChronoUnit.YEARS


  implicit val vitalStatusDE: ValueSet[VitalStatus.Value] =
    ValueSet(
      "Vital-Status",
      VitalStatus.Alive    -> "Lebend",
      VitalStatus.Deceased -> "Verstorben"
    )


  implicit val specimenAndReportToSummary: ((SomaticNGSReport,Option[Specimen])) => NGSSummary = {

    case (ngs,specimen) =>

      NGSSummary(
        ngs.patient,
        ngs.specimen,
        specimen.map(_.icd10.mapTo[ICD10Display]).toRight(NotAvailable),
        specimen.flatMap(_.`type`)
          .flatMap(ValueSet[Specimen.Type.Value].displayOf)
          .toRight(NotAvailable),
        specimen.flatMap(_.collection.map(_.localization))
          .flatMap(ValueSet[Specimen.Collection.Localization.Value].displayOf)
          .toRight(NotAvailable),
        ngs.sequencingType,
        ngs.tumorCellContent.map(_.mapTo[TumorCellContentDisplay]).toRight(NotAvailable)
      )

  }


  implicit val specimensAndReportsToSummaries: ((List[SomaticNGSReport],List[Specimen])) => List[NGSSummary] = {
    case (reports,specimens) =>
      reports.map(
        ngs =>
          (ngs -> specimens.find(_.id == ngs.specimen)).mapTo[NGSSummary]
      )
     
  }


  val formatter = DateTimeFormatter.ofPattern("MM.yyyy")


  implicit val patientAndDiagnosesToView: ((Patient,List[Diagnosis])) => PatientView = {
    case (pat,diagnoses) =>
      val diags =
        diagnoses.map(_.mapTo[DiagnosisView].icd10.toOption)
          .filter(_.isDefined)
          .map(_.get)
          .map(_.value)
          .reduceLeftOption(_ + ";\n" + _)
          .map(DiagnosisSummary(_))
          .toRight(NotAvailable)

      PatientView(
        pat.id,
        pat.managingZPM.toRight(NotAvailable),
        ValueSet[Gender.Value].displayOf(pat.gender).get,
        diags,
        pat.birthDate.map(
          bd => YEARS.between(bd,pat.dateOfDeath.getOrElse(LocalDate.now)).toInt
        )
        .toRight(NotAvailable),
        pat.dateOfDeath
          .map(formatter.format)
          .flatMap(
            d =>
              ValueSet[VitalStatus.Value].displayOf(VitalStatus.Deceased)
                .map(_ + s" ($d)")
          )
          .orElse(ValueSet[VitalStatus.Value].displayOf(VitalStatus.Alive))
          .get
      )

  }


}
object Mappings extends Mappings

