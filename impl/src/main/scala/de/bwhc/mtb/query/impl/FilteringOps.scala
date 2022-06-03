package de.bwhc.mtb.query.impl


import java.time.YearMonth
import de.bwhc.util.data.ClosedInterval
import de.bwhc.mtb.query.api.{
  Query,
  Selection,
  VitalStatus
}
import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  Patient,
  Coding,
  Gender,
  Medication,
  TherapyRecommendation,
  LevelOfEvidence,
  MolecularTherapy,
  StartedMolecularTherapy,
  Specimen,
  SomaticNGSReport,
  Variant,
  RECIST,
  Response,
  ValueSets
}
import de.bwhc.catalogs.med.MedicationCatalog



trait FilteringOps
{

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
    import de.bwhc.mtb.data.entry.dtos.ValueSets._

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


  private def DefaultTherapyRecommendationFilter(
    mtbfiles: Iterable[MTBFile]
  )(
    implicit catalog: MedicationCatalog
  ): Query.TherapyRecommendationFilter = {

    import ValueSets._

    val recommendations =
      mtbfiles.flatMap(_.recommendations.getOrElse(List.empty))
        .toList

    val medicationGroups =
      recommendations.flatMap(_.medication.getOrElse(List.empty))
        .distinctBy(_.code)
        .flatMap(coding => catalog.findWithCode(coding.code.value))
        .groupBy(_.parent.flatMap(catalog.find(_)))

    val priorities =
      recommendations.flatMap(_.priority)
        .toSet

    val evidenceLevels =
      recommendations.flatMap(_.levelOfEvidence.map(_.grading.code))
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
      medicationGroups.toList
        .map {
          case (group,medications) =>
            Selection(
              group.get.name,
              medications.map(
                med => Selection.Item(Coding(Medication.Code(med.code.value),Some(med.name)),true)
              )
            )
        },
    )

  }


  private def DefaultMolecularTherapyFilter(
    mtbfiles: Iterable[MTBFile]
  )(
    implicit catalog: MedicationCatalog
  ): Query.MolecularTherapyFilter = {

    import ValueSets._

    val therapies =
      mtbfiles.flatMap(_.molecularTherapies.getOrElse(List.empty))
        .flatMap(_.history.maxByOption(_.recordedOn))

    val statusSet =
      therapies.map(_.status).toSet

    val recordingDates =
      therapies.map(th => YearMonth.from(th.recordedOn))

    val medicationGroups =
      therapies.flatMap {
        case th: StartedMolecularTherapy => th.medication.getOrElse(List.empty)
        case _                           => List.empty
      }
      .toList
      .distinctBy(_.code)
      .flatMap(coding => catalog.findWithCode(coding.code.value))
      .groupBy(_.parent.flatMap(catalog.find(_)))

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
      medicationGroups.toList
        .map {
          case (group,medications) =>
            Selection(
              group.get.name,
              medications.map(
                med => Selection.Item(Coding(Medication.Code(med.code.value),Some(med.name)),true)
              )
            )
        },
      Selection(
        "Response",
        RECIST.values.toSeq
          .map(r => Selection.Item(Coding(r), responses contains r))
      )
    )

  }


  def DefaultFilters(
    mtbfiles: Iterable[MTBFile]
  )(
    implicit catalog: MedicationCatalog
  ): Query.Filters = {

    Query.Filters(
      DefaultPatientFilter(mtbfiles),
      DefaultNGSSummaryFilter(mtbfiles),
      DefaultTherapyRecommendationFilter(mtbfiles),
      DefaultMolecularTherapyFilter(mtbfiles)
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
      (pat.age.exists(filter.ageRange.contains)) &&
      (filter.vitalStatus.selectedValues.map(_.code) contains pat.vitalStatus)

  }


  implicit def ngsSummaryFilterToPredicate(
    filter: Query.NGSSummaryFilter
  ): ((SomaticNGSReport,Option[Specimen])) => Boolean = {

    case (ngsReport,specimen) =>

      val specimenTypes =
        filter.specimenType.selectedValues

      val specimenLocalizations =
        filter.specimenLocalization.selectedValues

      specimen.fold(
        specimenTypes.isEmpty && specimenLocalizations.isEmpty
      )(
        sp =>
          sp.`type`.exists(specimenTypes.contains) && 
          sp.collection.map(_.localization).exists(specimenLocalizations.contains)  
      ) &&
      ngsReport.tmb.exists(
        tmb => filter.tumorMutationalBurden.contains(tmb.value.toInt)
      )

  }


  implicit def therapyRecommendationFilterToPredicate(
    filter: Query.TherapyRecommendationFilter
  ): TherapyRecommendation => Boolean = {

    recommendation =>

      val selectedMedications =
        filter.medication.flatMap(_.selectedValues.map(_.code))

      recommendation.priority.exists(filter.priority.isSelected) &&
      recommendation.levelOfEvidence.map(_.grading.code).exists(filter.levelOfEvidence.isSelected) &&
      recommendation.medication.fold(selectedMedications.isEmpty)(_.exists(c => selectedMedications.contains(c.code)))
    
  }


  implicit def molecularTherapyFilterToPredicate(
    filter: Query.MolecularTherapyFilter
  ): ((MolecularTherapy,Option[Response])) => Boolean = {

    case (therapy,response) =>

      val responses = 
        filter.response.selectedValues.map(_.code)

      val selectedMedications =
        filter.medication.flatMap(_.selectedValues.map(_.code))
     
      filter.status.selectedValues.map(_.code).contains(therapy.status) &&
      filter.recordingDate.contains(YearMonth.from(therapy.recordedOn)) &&
      (therapy match {
        case th: StartedMolecularTherapy =>
          th.medication.getOrElse(List.empty)
            .exists(c => selectedMedications.contains(c.code))

        case _ => 
          selectedMedications.isEmpty
      }) &&
      response.map(_.value.code).fold(responses.isEmpty)(responses.contains)
      
  }

}
object FilteringOps extends FilteringOps