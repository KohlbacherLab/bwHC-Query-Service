package de.bwhc.mtb.query.impl


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors._
import org.scalatest.matchers.{
  BeMatcher,
  MatchResult
}
import org.scalatest.matchers.must.Matchers._
import de.bwhc.mtb.query.api.{
  ConceptCount,
  Report,
  LocalReport,
  GlobalReport,
  Query
}
import de.bwhc.mtb.query.api.ReportingAliases._
import de.bwhc.catalogs.med.MedicationCatalog
import de.bwhc.catalogs.icd.ICD10GMCatalogs
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  Medication,
  MTBFile,
  ZPM
}
import de.ekut.tbi.generators.Gen
import de.bwhc.mtb.dto.gens._
import de.bwhc.mtb.dto.extensions.CodingExtensions._
import play.api.libs.json.Json


sealed trait CustomMatchers {

  class MedicationReportMatcher
  extends BeMatcher[Report[Distribution[Medication.Coding]]]
  {
    override def apply(report: Report[Distribution[Medication.Coding]]) = {
 
      MatchResult(
        report.data
          .forall {
            case ConceptCount(group,n,optComponents) =>
       
              optComponents match {
                case Some(components) =>
                  components
                    .forall(_.concept.code.value.startsWith(group.code.value)) &&
                  (n == components.map(_.count).sum)
       
                case None => true
              }
          },
        s"Invalid MedicationDistributionReport: ${report}",
        s"Valid MedicationDistributionReport: ${report}"
//        s"Invalid MedicationDistributionReport: ${Json.prettyPrint(Json.toJson(report))}",
//        s"Valid MedicationDistributionReport: ${Json.prettyPrint(Json.toJson(report))}"
      )
    }
  }

  class TumorEntityReportMatcher
  extends BeMatcher[Report[Distribution[Coding[ICD10GM]]]]
  {
    override def apply(report: Report[Distribution[Coding[ICD10GM]]]) = {
 
      MatchResult(
        report.data
          .forall {
            case ConceptCount(group,n,optComponents) =>
       
              optComponents match {
                case Some(components) =>
                  components
                    .forall(_.concept.code.value.startsWith(group.code.value)) &&
                  (n == components.map(_.count).sum)
       
                case None => true
              }
          },
        s"Invalid TumorEntityDistributionReport: ${report}",
        s"Valid TumorEntityDistributionReport: ${report}"
//        s"Invalid TumorEntityDistributionReport: ${Json.prettyPrint(Json.toJson(report))}",
//        s"Valid TumorEntityDistributionReport: ${Json.prettyPrint(Json.toJson(report))}"
      )
    }
  }

  val validMedicationReport  = new MedicationReportMatcher
  val validTumorEntityReport = new TumorEntityReportMatcher
}




class TherapyReportingTests extends AnyFlatSpec
  with CustomMatchers
{

  import TherapyReportingOperations._
  import DistributionReportOperations._

  implicit val rnd =
    new scala.util.Random(42)

  implicit val atcCatalogs =
    MedicationCatalog.getInstance.get

  implicit val icd10catalogs =
    ICD10GMCatalogs.getInstance.get

  val zpm =
    ZPM("Dummy")


  private def mtbfiles(n: Int) =
    List.fill(n)(Gen.of[MTBFile].next)


  "Compiled LocalMedicationDistributionReport" must "have been valid" in {

    val report =
      toLocalMedicationDistributionReport(zpm,mtbfiles(50),Query.DrugUsage.Used)

    report must be (validMedicationReport)
  }


  "Compiled GlobalMedicationDistributionReport" must "have been valid" in {

    val report =
      List.fill(4)(mtbfiles(50))
        .map(
          toLocalMedicationDistributionReport(zpm,_,Query.DrugUsage.Used)
        )
        .combineToGlobalReport

    report must be (validMedicationReport)
  }


  "Compiled LocalTumorEntityDistributionReport" must "have been valid" in {

    val report =
      toLocalTumorEntityDistributionReport(zpm,mtbfiles(50))(Report.Filters.empty)

    report must be (validTumorEntityReport)
  }


  "Compiled GlobalTumorEntitDistributionReport" must "have been valid" in {

    val report =
      List.fill(4)(mtbfiles(50))
        .map(
          toLocalTumorEntityDistributionReport(zpm,_)(Report.Filters.empty)
        )
        .combineToGlobalReport

    report must be (validTumorEntityReport)
  }



  "Expansion of medication group coding to set containing all child substances" must "have worked" in {

     val set =
       expandToCodeset(
         Medication.Coding(
           code = Medication.Code("L01XX"),
           system = Medication.System.ATC,
           display = None,
           version = Some("2020")
         )
       )

     set.size must be > 1 

  }

/*
  "Compiled PatientTherapies" must "not be empty" in {

    val patientTherapiesReport =
      toPatientTherapies(
        zpm,
        mtbfiles(100)
      )(
        Report.Filters(
          Some(
            Medication.Coding(
              code = Medication.Code("L01XC"),
              system = Medication.System.ATC,
              display = None,
              version = Some("2020")
            )
          )
        )
      )

    patientTherapiesReport.data must not be (empty)  

  }
*/

  "Compiled PatientTherapies" must "not be empty" in {

    val cohort = mtbfiles(100)

    val medications =
      cohort
        .flatMap(_.molecularTherapies.getOrElse(List.empty))
        .map(_.history.maxBy(_.recordedOn))
        .flatMap(_.medication.getOrElse(List.empty))
        .distinctBy(_.code)

    val medicationGroups =
      medications.flatMap(_.medicationGroup)
        .distinctBy(_.code)
        
    forAll(medicationGroups ++ medications){
      medication =>

        val patientTherapiesReport =
          toPatientTherapies(
            zpm, cohort
          )(
            Report.Filters(
              Some(medication),
              None
            )
          )

      patientTherapiesReport.data must not be (empty)  
    }

  }

}
