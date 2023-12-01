package de.bwhc.mtb.query.impl


import java.time.YearMonth
import de.bwhc.util.data.ClosedInterval
import de.bwhc.mtb.query.api.{
  Query,
  Selection,
  VitalStatus
}
import de.bwhc.mtb.dtos.{
  MTBFile,
  Patient,
  Coding,
  Gender,
  Medication,
  TherapyRecommendation,
  LevelOfEvidence,
  MolecularTherapy,
  Specimen,
  SomaticNGSReport,
  Variant,
  RECIST,
  Response,
  ValueSets
}
import de.bwhc.catalogs.med.MedicationCatalog

import play.api.libs.json.Writes
import play.api.libs.json.Json.{prettyPrint,toJson}


trait FilteringOps
{

  import scala.util.chaining._


  def DefaultPatientFilter(
    mtbfiles: Iterable[MTBFile]
  ): Query.PatientFilter = {

    import de.bwhc.mtb.query.api.extensions._
    import ValueSets._


    val patients =
      mtbfiles.map(_.patient)

    val genders =
      patients.map(_.gender).toSet

    val ages =
      patients.flatMap(_.age)

    val ageRange =
      ClosedInterval(
        ages.minOption.getOrElse(0) -> ages.maxOption.getOrElse(0)
      )

    val vitalStatus = patients.map(_.vitalStatus).toSet

    Query.PatientFilter(
      Selection(
        "Geschlecht",
        Gender.values.toSeq
          .map(g => Selection.Item(Coding(g), genders contains g))
      ),
      ageRange,
      Selection(
        "Vital-Status",
        VitalStatus.values.toSeq
          .map(st => Selection.Item(Coding(st), vitalStatus contains st))
      )
    )

  }


  private def DefaultNGSSummaryFilter(
    mtbfiles: Iterable[MTBFile]
  ): Query.NGSSummaryFilter = {

    import scala.math.Ordering.Double.IeeeOrdering
    import de.bwhc.mtb.dtos.ValueSets._

    val specimens =
      mtbfiles.toList.flatMap(_.specimens.getOrElse(List.empty))

    val ngsReports =
      mtbfiles.toList.flatMap(_.ngsReports.getOrElse(List.empty))

    val (specimenTypes,specimenLocalizations) =
      specimens.foldLeft(
        (Set.empty[Specimen.Type.Value], Set.empty[Specimen.Collection.Localization.Value])
      ){
        case ((types,localizations),sp) =>
          (types ++ sp.`type`, localizations ++ sp.collection.map(_.localization))
      }

    val tmbRange =
      ngsReports.flatMap(_.tmb.map(_.value.toInt).toList)


    Query.NGSSummaryFilter(
      Selection(
        "Proben-Art",
        Specimen.Type.values.toSeq
          .map(t => Selection.Item(Coding(t), specimenTypes contains t))
      ),
      Selection(
        "Proben-Lokalisierung",
        Specimen.Collection.Localization.values.toSeq
          .map(t => Selection.Item(Coding(t), specimenLocalizations contains t))
      ),
      ClosedInterval(
        tmbRange.minOption.getOrElse(0) -> tmbRange.maxOption.getOrElse(0)
      )
    )

  }


  private def toCoding(
    implicit catalog: MedicationCatalog
  ): Medication.Coding => Option[Coding[Medication.Code]] = {

    case Medication.Coding(code,system,display,version) =>

      system match {

        case Medication.System.ATC =>
          catalog.findWithCode(
            code.value,
            version.getOrElse(catalog.latestVersion)
          )
          .map(
            med =>
              Coding(
                Medication.Code(med.code.value),
                Some(med.name),
                version
              )
          )

        case Medication.System.Unregistered =>
          Some(
            Coding(
              code,
              display.orElse(Some("N/A")),
              version
            )
          )
      }

  }


  private val EmptyMedication =
    Medication.Coding(
      Medication.Code("_empty-medication_"),
      Medication.System.Unregistered,
      Some("Keine Medikation (N/A)"),
      Some("-")
    ) 


  private def DefaultTherapyRecommendationFilter(
    mtbfiles: Iterable[MTBFile],
    queriedMedications: List[Coding[Medication.Code]]
  )(
    implicit catalog: MedicationCatalog
  ): Query.TherapyRecommendationFilter = {

    import ValueSets._

    val recommendations =
      mtbfiles
        .flatMap(_.recommendations.getOrElse(List.empty))
        .toList

    val medications =
      recommendations
        .toList
        .flatMap(_.medication.getOrElse(List.empty))
        .distinctBy(_.code)
        .sortBy(_.code.value)
        .pipe(
          meds =>
            if (recommendations.exists(_.medication.isEmpty))
              EmptyMedication :: meds
            else
              meds
        )
        .flatMap(toCoding)
        


    val priorities =
      recommendations
        .flatMap(_.priority)
        .toSet

    val evidenceLevels =
      recommendations
        .flatMap(_.levelOfEvidence.map(_.grading.code))
        .pipe { 
          loe =>
            if (recommendations.exists(_.levelOfEvidence.isEmpty))
              LevelOfEvidence.Grading.Undefined :: loe
            else
              loe
        }
        .toSet


    Query.TherapyRecommendationFilter(
      Selection(
        "Priorität",
        TherapyRecommendation.Priority.values.toSeq
          .map(p => Selection.Item(p, priorities contains p))
      ),
      Selection(
        "Evidenzgrad",
        LevelOfEvidence.Grading.values.toSeq
          .map(l => Selection.Item(l, evidenceLevels contains l))
      ),
      Selection(
        "Empfohlene Medikationen",
        medications
          .map(coding =>
            Selection.Item(
              coding,
              queriedMedications.isEmpty || queriedMedications.exists(_.code == coding.code)
            )
          )
      )  
    )

  }


  private def DefaultMolecularTherapyFilter(
    mtbfiles: Iterable[MTBFile],
    queriedMedications: List[Coding[Medication.Code]]
  )(
    implicit catalog: MedicationCatalog
  ): Query.MolecularTherapyFilter = {

    import ValueSets._

    val therapies =
      mtbfiles
        .flatMap(_.molecularTherapies.getOrElse(List.empty))
        .flatMap(_.history.maxByOption(_.recordedOn))

    val statusSet =
      therapies.map(_.status).toSet

    val recordingDates =
      therapies.map(th => YearMonth.from(th.recordedOn))


    val medications =
      therapies
        .toList
        .flatMap(_.medication.getOrElse(List.empty))
        .distinctBy(_.code)
        .sortBy(_.code.value)
        .pipe(
          meds =>
            if (therapies.exists(_.medication.isEmpty))
              EmptyMedication :: meds
            else
              meds
        )
        .flatMap(toCoding)

    val responses =
      mtbfiles.flatMap(_.responses.getOrElse(List.empty))
        .map(_.value.code)
        .toSet

    Query.MolecularTherapyFilter(
      Selection(
        "Therapie-Status",
        MolecularTherapy.Status.values.toSeq
          .map(st => Selection.Item(Coding(st), statusSet contains st))
      ),
      ClosedInterval(
        recordingDates.minOption.getOrElse(YearMonth.now) -> recordingDates.maxOption.getOrElse(YearMonth.now)
      ),
      Selection(
        "Verabreichte Medikationen",
        medications
          .map(coding =>
            Selection.Item(
              coding,
              queriedMedications.isEmpty || queriedMedications.exists(_.code == coding.code)
            )
          )
      ),
      Selection(
        "Response",
        RECIST.values.toSeq
          .map(r => Selection.Item(Coding(r), responses contains r))
      )
    )

  }


  def DefaultFilters(
    mtbfiles: Iterable[MTBFile],
    parameters: Query.Parameters
  )(
    implicit catalog: MedicationCatalog
  ): Query.Filters = {

    Query.Filters(
      DefaultPatientFilter(mtbfiles),
      DefaultNGSSummaryFilter(mtbfiles),
      DefaultTherapyRecommendationFilter(
        mtbfiles,
        parameters
          .medicationsWithUsage
          .getOrElse(List.empty)
          .filter { mwu => 
            val usage = mwu.usage
             
            usage.isEmpty || usage.map(_.code).contains(Query.DrugUsage.Recommended)
          }
          .map(_.medication)
      ),
      DefaultMolecularTherapyFilter(
        mtbfiles,
        parameters
          .medicationsWithUsage
          .getOrElse(List.empty)
          .filter { mwu => 
            val usage = mwu.usage
             
            usage.isEmpty || usage.map(_.code).contains(Query.DrugUsage.Used)
          }
          .map(_.medication)
      )
    )

  }



  import scala.language.implicitConversions

  implicit def patientFilterToPredicate(
    filter: Query.PatientFilter
  ): MTBFile => Boolean = {

    import de.bwhc.mtb.query.api.extensions._

    mtbfile =>

      val pat = mtbfile.patient
      
      (filter.gender.selectedValues.map(_.code) contains pat.gender) &&
      (pat.age.fold(true)(filter.ageRange.contains)) &&
      (filter.vitalStatus.selectedValues.map(_.code) contains pat.vitalStatus)

  }


  implicit def ngsSummaryFilterToPredicate(
    filter: Query.NGSSummaryFilter
  ): ((SomaticNGSReport,Option[Specimen])) => Boolean = {

    case (ngsReport,specimen) =>

      val specimenTypes: Seq[Specimen.Type.Value] =
        filter.specimenType.selectedValues.map(_.code)
      
      val specimenLocalizations: Seq[Specimen.Collection.Localization.Value] =
        filter.specimenLocalization.selectedValues.map(_.code)
      
      specimen.fold(
        true
      )(
        sp =>
          sp.`type`.fold(true)(specimenTypes.contains) && 
          sp.collection.map(_.localization).fold(true)(specimenLocalizations.contains)  
      ) &&
      ngsReport.tmb.fold(true)(tmb => filter.tumorMutationalBurden.contains(tmb.value.toInt))
  }


  implicit def therapyRecommendationFilterToPredicate(
    filter: Query.TherapyRecommendationFilter
  ): TherapyRecommendation => Boolean = {

    recommendation =>

      val selectedMedicationCodes: Seq[Medication.Code] =
        filter.medication.selectedValues.map(_.code)
      
      recommendation.priority.fold(true)(filter.priority.isSelected(_)) &&
      filter.levelOfEvidence
        .selectedValues
        .contains(
          recommendation
            .levelOfEvidence
            .map(_.grading.code)
            .getOrElse(LevelOfEvidence.Grading.Undefined)
        ) &&
      recommendation.medication
        .fold(
          selectedMedicationCodes.contains(EmptyMedication.code)
        )(
          _.exists(c => selectedMedicationCodes contains c.code)
        )

  }


  implicit def molecularTherapyFilterToPredicate(
    filter: Query.MolecularTherapyFilter
  ): ((MolecularTherapy,Option[Response])) => Boolean = {

    case (therapy,response) =>

      val responses: Seq[RECIST.Value] = 
        filter.response.selectedValues.map(_.code)
      
      val selectedMedicationCodes: Seq[Medication.Code] =
        filter.medication.selectedValues.map(_.code)
      
      filter.status.selectedValues.map(_.code).contains(therapy.status) &&
      filter.recordingDate.contains(YearMonth.from(therapy.recordedOn)) &&
      therapy.medication
        .fold(
          selectedMedicationCodes.contains(EmptyMedication.code)
        )(
          _.exists(c => selectedMedicationCodes.contains(c.code))
        ) &&
      response.map(_.value.code).fold(true)(responses.contains)

  }

}
object FilteringOps extends FilteringOps
