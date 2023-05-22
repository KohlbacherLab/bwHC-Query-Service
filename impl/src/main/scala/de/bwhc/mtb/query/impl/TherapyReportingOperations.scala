package de.bwhc.mtb.query.impl



import java.time.LocalDateTime
import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICD10GM,
  Medication,
  MolecularTherapy,
  MTBFile,
  ZPM
}
import de.bwhc.mtb.query.api.{
  ConceptCount,
  Report,
  LocalReport,
  GlobalReport,
  PatientTherapies,
  TherapyWithResponse
}
import de.bwhc.mtb.query.api.ReportingAliases._
import de.bwhc.catalogs.med.{
  MedicationCatalog => ATCCatalog,
  Medication => ATCMedication
}
import de.bwhc.catalogs.icd.{
  ICD10GMCoding,
  ICD10GMCatalogs,
  ICD10GM => ICD10
}


object ConceptCountOperations
{


  implicit class ConceptCountExtensions[T](val that: ConceptCount[T]) extends AnyVal
  {

    // Define addition of two ConceptCount[T] instances
    def +(other: ConceptCount[T]): ConceptCount[T] = {

      if (that.concept == other.concept)
        ConceptCount[T](
          that.concept,
          that.count + other.count,
          (that.components,other.components) match {
            case (Some(thatCs),Some(otherCs)) => Some(addUp(thatCs,otherCs))
            case (Some(thatCs),None)          => Some(thatCs)
            case (None,Some(otherCs))         => Some(otherCs)
            case (None,None)                  => None
          }
        )
      else
        that
    }

  }

  // Adding up of ConceptCounts
  def addUp[T](
    ccs1: Seq[ConceptCount[T]],
    ccs2: Seq[ConceptCount[T]]
  ): Seq[ConceptCount[T]] = 
    (ccs1 ++ ccs2)
      .groupMapReduce(_.concept)(identity)(_ + _)
      .values
      .toSeq

}


object DistributionReportOperations
{


  implicit class DistributionReportExtensions[T](
    val parts: Seq[LocalDistributionReport[T]]
  ) extends AnyVal {

    def combineToGlobalReport: GlobalDistributionReport[T] = {
    
      import ConceptCountOperations._
    
      GlobalReport[Distribution[T]](
        LocalDateTime.now,
        parts.map(_.site).toList,
        parts.head.filters,
        parts.foldLeft(
          Seq.empty[ConceptCount[T]]
        )(
          (acc,report) => addUp(acc,report.data)
        ),
        Some(parts)
      )
    }

  }

}


trait TherapyReportingOperations
{

  private val toCoding: ATCMedication => Medication.Coding =
    concept => 
      Medication.Coding(
        Medication.Code(concept.code.value),
        Medication.System.ATC,
        Some(concept.name),
        Some(concept.version)
      )


  private def resolveWithParent(
    coding: Medication.Coding
  )(
    implicit
    catalogs: ATCCatalog
  ): (Medication.Coding,Medication.Coding) = {

    val atcEntry =
      catalogs.findWithCode(
        coding.code.value,
        coding.version.get  // Safe: ensured defined by data validation
      )
      .get
         
    val atcClass =
      atcEntry.parent match {
        case Some(code) =>
          catalogs.find(code,atcEntry.version)
            .get   // Safe
        case None =>
          atcEntry
      }

    (toCoding(atcClass) -> toCoding(atcEntry))
  }


  private val ATCVersionOrder =
    Ordering.by((v: String) => v.toInt)

  def toLocalMedicationDistributionReport(
    site: ZPM,
    mtbfiles: Iterable[MTBFile]
  )(
    implicit 
    atcCatalogs: ATCCatalog
  ): LocalDistributionReport[Medication.Coding] = {

    import scala.collection.mutable.Map

    val conceptCounts =
      mtbfiles.flatMap(
        _.molecularTherapies
         .getOrElse(List.empty)
         .map(_.history.maxBy(_.recordedOn))
         .flatMap(
           _.medication
            .getOrElse(List.empty)
            .filter(_.system == Medication.System.ATC)
         )
      )
      .groupBy(_.code)
      .values
      .map {
        meds =>

          // Treat medication codings with same code but different version
          // as being conceptually continuous across subsequent versions,
          // i.e. being the same entry at the version maximum (i.e. latest version),
          // to avoid seeming duplication by having
          // e.g. medication (code,2020), (code,2021), (code,2022) 
          // in different "bins", and instead count them as all being (code,2022)
          val maxByVersion =
            meds.maxBy(_.version.getOrElse(atcCatalogs.latestVersion))(ATCVersionOrder)

          val (medicationClass,medication) =
            resolveWithParent(maxByVersion)

          (medicationClass -> (medication,meds.size))
      }
      // group by medication class, mapping the values to medications with their occurrence
      .groupMap(_._1)(_._2)
      .map {
        case (medicationClass,medicationsWithCount) =>
          ConceptCount(
            medicationClass,
            medicationsWithCount.map(_._2).sum,
            Some(
              medicationsWithCount.map {
                case (medication,n) =>
                  ConceptCount(
                    medication,
                    n,
                    None
                  )
              }
              .toSeq
              .sortWith(_.count > _.count)
            )
          )
      }
      .toSeq
      .sortWith(_.count > _.count)

    LocalReport[Distribution[Medication.Coding]](
      LocalDateTime.now,
      site,
      Report.Filters.empty,
      conceptCounts
    )

  }


  private def toCoding(
    code: ICD10.Code
  )(
    implicit
    catalogs: ICD10GMCatalogs
  ): Coding[ICD10GM] = {
    catalogs.coding(code)
      .map(
        concept =>
          Coding(
          ICD10GM(concept.code.value),
          Some(concept.display),
          None // erase version as all codes are combined
        )
      )
      .get // safe
  }    
          
  private def resolveWithSuperClass(
    coding: Coding[ICD10GM]
  )(
    implicit
    catalogs: ICD10GMCatalogs
  ): (Coding[ICD10GM],Coding[ICD10GM]) = {

    val category =
      catalogs.coding(
        ICD10.Code(coding.code.value),
        coding.version.get  // Safe: ensured defined by data validation
      )
      .get

    toCoding(category.superClass.getOrElse(category.code)) -> toCoding(category.code)
  }



  private def resolve(
    medication: Medication.Coding
  )(
    implicit 
    atcCatalogs: ATCCatalog
  ): Option[Medication.Coding] = 
    atcCatalogs.findWithCode(
      medication.code.value,
      medication.version.get  // Safe here
    )    
    .map(toCoding)


  // Expand medication to include substances if it's a medication class/group
  private def expandToCodeset(
    medication: Medication.Coding
  )(
    implicit 
    atcCatalogs: ATCCatalog
  ): Set[Medication.Code] =
    atcCatalogs.findWithCode(
      medication.code.value,
      medication.version.get  // Safe here
    )    
    .map(
      atcEntry =>
        Set(Medication.Code(atcEntry.code.value)) ++ (
          atcEntry.kind match {
            case ATCMedication.Kind.Group =>
              atcEntry.children.map(code => Medication.Code(code.value))
       
            case ATCMedication.Kind.Substance =>
              Set.empty[Medication.Code]
          }
       )
     )
     .get


  private val ICD10VersionOrder =
    Ordering.by((v: String) => v.toInt)

  def toLocalTumorEntityDistributionReport(
    site: ZPM,
    mtbfiles: Iterable[MTBFile]
  )(
    filters: Report.Filters
  )(
    implicit 
    atcCatalogs: ATCCatalog,
    icd10catalogs: ICD10GMCatalogs
  ): LocalDistributionReport[Coding[ICD10GM]] = {

    import scala.collection.mutable.Map

    val medicationCodeFilter =
      filters.medication.map(expandToCodeset)
 
    val diagnoses =
      medicationCodeFilter match {

        // If medication codes are defined as filter,
        // pick only diagnoses (tumor entities) for which
        // a therapy with the given medication codes exists
        case Some(codes) => {
           mtbfiles.flatMap(
             mtbfile =>
               mtbfile.molecularTherapies
                .getOrElse(List.empty)
                .map(_.history.maxBy(_.recordedOn))
                // retain only therapies in which the given medication was used
                .withFilter {
                  _.medication match {
                    case Some(meds) => meds.exists(med => codes.contains(med.code))
                    case None => false
                  }
                }
                // Resolve diagnoses the therapy was an indication for
                .flatMap(
                  therapy =>
                    mtbfile.recommendations
                      .flatMap(_.find(_.id == therapy.basedOn))
                      .flatMap(rec => mtbfile.diagnoses.flatMap(_.find(_.id == rec.diagnosis)))
                )
           )
        }

        // Else use all diagnoses (tumor entities)
        case None =>
          mtbfiles.flatMap(_.diagnoses.getOrElse(List.empty))
      }

    val conceptCounts =
      diagnoses
        .flatMap(_.icd10)
        .groupBy(_.code)
        .values
        .map {
          codings =>
            val maxByVersion =
              codings.maxBy(_.version.getOrElse(icd10catalogs.latestVersion))(ICD10VersionOrder)

            val (superClass,category) =
              resolveWithSuperClass(maxByVersion)

            superClass -> (category -> codings.size)
        }
        // group by ICD10 superclass, mapping the values to ICD categories with their occurrence
        .groupMap(_._1)(_._2)
        .map {
          case (superClass,categoriesWithCount) =>
            ConceptCount(
              superClass,
              categoriesWithCount.map(_._2).sum,
              Some(
                categoriesWithCount.map {
                  case (category,n) =>
                    ConceptCount(
                      category,
                      n,
                      None
                    )
                }
                .toSeq
                .sortWith(_.count > _.count)
              )
            )
        }
        .toSeq
        .sortWith(_.count > _.count)

    LocalReport[Distribution[Coding[ICD10GM]]](
      LocalDateTime.now,
      site,
      filters.copy(
        medication = filters.medication.flatMap(resolve)
      ),
      conceptCounts
    )

  }


  def toPatientTherapies(
    site: ZPM,
    mtbfiles: Iterable[MTBFile]
  )(
    filters: Report.Filters
  )(
    implicit 
    atcCatalogs: ATCCatalog,
    icd10catalogs: ICD10GMCatalogs
  ): LocalReport[Seq[PatientTherapies]] = {

    import de.bwhc.mtb.data.entry.views.mappings.supportingVariantToDisplay

    val therapyFilter: MolecularTherapy => Boolean =
      filters.medication
        .map(expandToCodeset)
        .map[MolecularTherapy => Boolean](
          codes => 
            therapy => therapy.medication match {
              case Some(meds) => meds.exists(med => codes.contains(med.code))
              case None => false
            }
        )
        .getOrElse(th => true)


    LocalReport(
      LocalDateTime.now,
      site,
      filters.copy(
        medication = filters.medication.flatMap(resolve)
      ),
      mtbfiles.map(
        mtbfile =>

          PatientTherapies(
            mtbfile.patient,
            mtbfile.molecularTherapies
              .getOrElse(List.empty)
              .flatMap(_.history.maxByOption(_.recordedOn))
              // retain only therapies in which the given medication was used
              .withFilter(therapyFilter)
              .map {
                therapy =>

                  val icd10 =
                    mtbfile.recommendations
                      .flatMap(_.find(_.id == therapy.basedOn))
                      .flatMap(rec => mtbfile.diagnoses.flatMap(_.find(_.id == rec.diagnosis)))
                      .flatMap(_.icd10)
                      .map(c => toCoding(ICD10.Code(c.code.value)))

                  TherapyWithResponse(
                    therapy.copy(reason = icd10),
                    for {
                      recommendation <-
                        mtbfile.recommendations
                          .flatMap(_.find(_.id == therapy.basedOn))

                      ngsReport <-
                        recommendation.ngsReport
                          .flatMap(ref => mtbfile.ngsReports.getOrElse(List.empty).find(_.id == ref))

                      supportingVariants = 
                        ngsReport.variants
                          .withFilter(variant => recommendation.supportingVariants.exists(_ contains variant.id))

                    } yield {
                      supportingVariants.map(supportingVariantToDisplay)
                    },
                    mtbfile.responses
                      .flatMap(_.filter(_.therapy == therapy.id).maxByOption(_.effectiveDate))
                  )

              }
          )
      )
      .filter(_.therapies.nonEmpty)
      .toSeq
    )

  }

}


object TherapyReportingOperations extends TherapyReportingOperations
