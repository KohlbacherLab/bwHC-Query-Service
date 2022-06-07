package de.bwhc.mtb.query.api


import java.time.LocalDate
import java.time.format.DateTimeFormatter
import cats.syntax.either._
import de.bwhc.mtb.data.entry.dtos.{
  Specimen,
  SomaticNGSReport,
  Patient,
  Diagnosis,
  Gender,
  ValueSet,
  Coding,
  TherapyRecommendation,
  MolecularTherapy,
  NotDoneTherapy,
  StoppedTherapy,
  CompletedTherapy,
  OngoingTherapy,
  Response,
  ICD10GM,
  Variant,
  ECOGStatus,
  RECIST,
  Dosage
}
import de.bwhc.mtb.data.entry.views.{
  DiagnosisView,
  ECOGDisplay,
  ICD10Display,
  NotAvailable,
  Undefined,
  TumorCellContentDisplay,
  LevelOfEvidenceDisplay,
  SupportingVariantDisplay,
  MedicationDisplay,
  ResponseDisplay,
  PeriodDisplay
}


trait Mappings
{

  import de.bwhc.mtb.data.entry.views.mappings._

  import de.bwhc.mtb.data.entry.dtos.ValueSets._

  import java.time.temporal.ChronoUnit.YEARS


  val formatter =
    DateTimeFormatter.ofPattern("MM.yyyy")


  implicit val patientAndDiagnosesToView:
  (
   (
    Patient,
    List[Diagnosis]
   )
  ) => PatientView = {

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


  implicit val specimenAndReportToSummary:
  (
   (
    SomaticNGSReport,
    Option[Specimen]
   )
  ) => NGSSummary = {

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


  implicit val recommendationToSummary:
   (
    (
     TherapyRecommendation,
     Option[Diagnosis],
     Option[ECOGStatus],
     List[Variant]
    )
   ) => TherapyRecommendationSummary = {


    case (rec,diagnosis,ecog,variants) =>

      val (medication,medicationClasses) =
        rec.medication
          .map(_.mapTo[(MedicationDisplay,MedicationDisplay)]).unzip

      TherapyRecommendationSummary(
        rec.id,
        rec.patient,
        diagnosis.flatMap(_.icd10).map(_.mapTo[ICD10Display]).toRight(NotAvailable),
        ecog.map(_.value.mapTo[ECOGDisplay]).toRight(NotAvailable),
        medication.toRight(NotAvailable),
        medicationClasses.toRight(NotAvailable),
        rec.priority.toRight(NotAvailable),
        rec.levelOfEvidence.map(_.mapTo[LevelOfEvidenceDisplay]).toRight(NotAvailable),
        variants.map(_.mapTo[SupportingVariantDisplay])
      )
  }


  implicit val molecularTherapyToSummary:
  (
   (
    MolecularTherapy,
    Option[Diagnosis],
    Option[TherapyRecommendation],
    List[Variant],
    Option[Response]
   )
  ) => MolecularTherapySummary = {

    case (molTh,diag,recommendation,variants,resp) =>

    val status   = ValueSet[MolecularTherapy.Status.Value].displayOf(molTh.status).get
    val note     = molTh.note.getOrElse("-")
    val icd10    = diag.flatMap(_.icd10).map(_.mapTo[ICD10Display]).toRight(NotAvailable)
    val priority = recommendation.flatMap(_.priority).toRight(NotAvailable)
    val levelOfEvidence = recommendation.flatMap(_.levelOfEvidence).map(_.grading.code).toRight(NotAvailable)

    val suppVariantDisplay = variants.map(_.mapTo[SupportingVariantDisplay])

    val response = resp.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable)
    val progressionDate = resp.filter(_.value.code == RECIST.PD).map(_.effectiveDate).toRight(Undefined)

    molTh match {

      case th: NotDoneTherapy =>
        MolecularTherapySummary(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          priority,
          levelOfEvidence,
          NotAvailable.asLeft[PeriodDisplay[LocalDate]],
          ValueSet[MolecularTherapy.NotDoneReason.Value].displayOf(th.notDoneReason.code).toRight(NotAvailable),
          Undefined.asLeft[MedicationDisplay],
          Undefined.asLeft[MedicationDisplay],
          suppVariantDisplay,
          Undefined.asLeft[String],
          NotAvailable.asLeft[Dosage.Value],
          note,
          response,
          progressionDate
        )

      case th: StoppedTherapy => {

        val (medication,medicationClasses) =
          th.medication
            .map(_.mapTo[(MedicationDisplay,MedicationDisplay)]).unzip

        MolecularTherapySummary(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          priority,
          levelOfEvidence,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          medication.toRight(NotAvailable),
          medicationClasses.toRight(NotAvailable),
          suppVariantDisplay,
          ValueSet[MolecularTherapy.StopReason.Value].displayOf(th.reasonStopped.code).toRight(NotAvailable),
          th.dosage.toRight(NotAvailable),
          note,
          response,
          progressionDate
        )
      }

     case th: CompletedTherapy => {

        val (medication,medicationClasses) =
          th.medication
            .map(_.mapTo[(MedicationDisplay,MedicationDisplay)]).unzip

        MolecularTherapySummary(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          priority,
          levelOfEvidence,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
        medication.toRight(NotAvailable),
        medicationClasses.toRight(NotAvailable),
          suppVariantDisplay,
          Undefined.asLeft[String],
          th.dosage.toRight(NotAvailable),
          note,
          response,
          progressionDate
        )
      }

      case th: OngoingTherapy => {

        val (medication,medicationClasses) =
          th.medication
            .map(_.mapTo[(MedicationDisplay,MedicationDisplay)]).unzip

        MolecularTherapySummary(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          priority,
          levelOfEvidence,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
        medication.toRight(NotAvailable),
        medicationClasses.toRight(NotAvailable),
          suppVariantDisplay,
          Undefined.asLeft[String],
          th.dosage.toRight(NotAvailable),
          note,
          response,
          progressionDate
        )
      }
    }
  }



  

}

object Mappings extends Mappings

