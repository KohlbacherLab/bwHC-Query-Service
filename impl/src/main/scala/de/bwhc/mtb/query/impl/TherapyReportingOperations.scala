package de.bwhc.mtb.query.impl



import java.time.LocalDateTime
import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICD10GM,
  Medication,
  MolecularTherapy,
//  StartedMolecularTherapy,
  MTBFile,
  ZPM
}
import de.bwhc.mtb.query.api.{
  ConceptCount,
  Report,
  LocalReport,
  GlobalReport,
  PatientTherapies,
  TherapyResponse
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

    val medication =
      catalogs.findWithCode(
        coding.code.value,
        coding.version.get  // Safe: ensured defined by data validation
      )
      .get
         
    val group =
      medication.parent match {
        case Some(code) =>
          catalogs.find(code,medication.version)
            .get   // Safe
        case None =>
          medication
      }

    (toCoding(group) -> toCoding(medication))
  }


  def toLocalMedicationDistributionReport(
    site: ZPM,
    mtbfiles: Iterable[MTBFile]
  )(
    implicit 
    catalogs: ATCCatalog
  ): LocalDistributionReport[Medication.Coding] = {

    import scala.collection.mutable.Map

    val conceptCounts =
      mtbfiles.flatMap(
        _.molecularTherapies
         .getOrElse(List.empty)
         .map(_.history.maxBy(_.recordedOn))
      )
      // Collect hierarchical counts of occurring Medication.Codings
      .foldLeft(
        Map.empty[Medication.Coding,Map[Medication.Coding,Int]]
      ){
        (groupCounts,therapy) =>
 
          for {
            med <- therapy.medication.getOrElse(List.empty)
/*
              therapy match {
                case th: StartedMolecularTherapy => th.medication.getOrElse(List.empty)
                case _                           => List.empty
              }
*/
            if med.system == Medication.System.ATC
 
            (group,medication) = resolveWithParent(med)
          }{
            groupCounts.updateWith(group){
              case Some(medicationCounts) =>
                Some(
                  medicationCounts +=
                    medication -> (medicationCounts.getOrElse(medication,0)+1)
                )
 
              case None =>
                Some(Map(medication -> 1))
            }
          }
          groupCounts
      }
      // Map to ConceptCount
      .map {
        case (group,medicationCounts) =>
          ConceptCount(
            group,
            medicationCounts.foldLeft(0){
              case (acc,(_,n)) => acc + n
            },
            Some(
              medicationCounts.map {
                case (medication,n) =>
                  ConceptCount(
                    medication,
                    n,
                    None
                  )
              }
              .toSeq
            )
          )
      }
      .toSeq
      
    LocalReport[Distribution[Medication.Coding]](
      LocalDateTime.now,
      site,
      Report.Filters.empty,
      conceptCounts.map(
        cc =>
          cc.copy(
            components = cc.components.map(_.sorted)
          )
      )
      .sorted
    )

  }



  private def resolveWithSuperCategory(
    coding: Coding[ICD10GM]
  )(
    implicit
    catalogs: ICD10GMCatalogs
  ): (ICD10.Code,ICD10.Code) = {

    val category =
      catalogs.coding(
        ICD10.Code(coding.code.value),
        coding.version.get  // Safe: ensured defined by data validation
      )
      .get

    category.superClass.getOrElse(category.code) -> category.code
  }

  private def resolveAsCoding(
    code: ICD10.Code
  )(
    implicit
    catalogs: ICD10GMCatalogs
  ): Coding[ICD10GM] =
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

        // If medication codes are defined,
        // pick only diagnoses (tumor entities) for which a therapy with the given medication codes exists
        case Some(codes) => {

           mtbfiles.flatMap(
             mtbfile =>
               mtbfile.molecularTherapies
                .getOrElse(List.empty)
                .map(_.history.maxBy(_.recordedOn))
                // retain only therapies in which the given medication was used
                .filter {
                  _.medication match {
                    case Some(meds) => meds.exists(med => codes.contains(med.code))
                    case None => false
                  }
/*                  
                  case th: StartedMolecularTherapy =>
                    th.medication
                      .getOrElse(List.empty)
                      .exists(med => codes.contains(med.code))
           
                  case _ => false
*/                  
                }
                // Resolve diagnoses the therapy was an indication for
                .flatMap(
                  therapy =>
                    mtbfile.recommendations
                      .flatMap(
                        _.find(_.id == therapy.basedOn)
                      )
                      .flatMap(
                        rec => mtbfile.diagnoses.flatMap(_.find(_.id == rec.diagnosis))
                      )
                )
           )

        }

        // Else use all diagnoses (tumor entities)
        case None =>
          mtbfiles.flatMap(_.diagnoses.getOrElse(List.empty))

      }

    val conceptCounts =
      // Collect hierarchical counts of occurring ICD-10 codes
      diagnoses
        .foldLeft(
          Map.empty[ICD10.Code,Map[ICD10.Code,Int]]
        ){
          (superCategoryCounts,diagnosis) =>
        
            diagnosis.icd10 match {
              case Some(coding) => {
                val (superCategory,category) = resolveWithSuperCategory(coding)
        
                superCategoryCounts.updateWith(superCategory){
                  case Some(categoryCounts) =>
                    Some(
                      categoryCounts +=
                        category -> (categoryCounts.getOrElse(category,0)+1)
                    )
        
                  case None =>
                    Some(Map(category -> 1))
                }
        
                superCategoryCounts
              }
        
              // In case ICD-10 Coding not defined on the Diagnosis
              case None => superCategoryCounts
            }
        }
        .map {
          case (superCategory,categoryCounts) =>
        
            ConceptCount(
              resolveAsCoding(superCategory),
              categoryCounts.foldLeft(0){
                case (acc,(_,n)) => acc + n
              },
              Some(
                categoryCounts.map {
                  case (category,n) =>
                    ConceptCount(
                      resolveAsCoding(category),
                      n,
                      None
                    )
                }
                .toSeq
              )
            )
        }
        .toSeq
      
    LocalReport[Distribution[Coding[ICD10GM]]](
      LocalDateTime.now,
      site,
      filters.copy(
        medication = filters.medication.flatMap(resolve)
      ),
      conceptCounts.map(
        cc =>
          cc.copy(
            components = cc.components.map(_.sorted)
          )
      )
      .sorted
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
/*          
          codes => {
            case th: StartedMolecularTherapy =>
               // retain only therapies in which the given medication was used
               th.medication
                 .getOrElse(List.empty)
                 .exists(med => codes contains med.code)
        
            case _ => false
          }
*/          
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

                  TherapyResponse(
                    therapy,
                    for {
                      recommendation <-
                        mtbfile.recommendations
                          .flatMap(_.find(_.id == therapy.basedOn))

                      ngsReport <-
                        recommendation.ngsReport
                          .flatMap(ref => mtbfile.ngsReports.getOrElse(List.empty).find(_.id == ref))

                      supportingVariants = 
                        ngsReport.variants
                          .filter(v => recommendation.supportingVariants.exists(_ contains v.id))

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


