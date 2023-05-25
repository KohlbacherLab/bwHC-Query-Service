package de.bwhc.mtb.query.test


import java.time.LocalDateTime
import java.nio.file.Files.createTempDirectory
import scala.math.Ordering.Double.TotalOrdering
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  Gender,
  ZPM,
  ValueSet,
  LevelOfEvidence,
  Specimen
}
import de.bwhc.util.data.{Interval,ClosedInterval}
import Interval._
import de.bwhc.mtb.query.api._
import Query._
import QueryOps.Command._
import QCReport._
import de.bwhc.catalogs.icd.{ICD10GMCatalogs,ICDO3Catalogs}


object Setup
{

  val tmpDir = createTempDirectory("bwHC_query_test").toFile

  tmpDir.deleteOnExit

  val N = 50;

  val localSite = ZPM("Tübingen")
 
  implicit val querier   = Querier("Dummy")

  System.setProperty("bwhc.zpm.site",localSite.value)
  System.setProperty("bwhc.query.data.dir", tmpDir.getAbsolutePath)
  System.setProperty("bwhc.query.data.generate", N.toString)

  lazy val serviceTry = QueryService.getInstance

  val icd10s =
    ICD10GMCatalogs.getInstance.get.codings()

}


class Tests extends AsyncFlatSpec
{

  import Setup._


  "QueryService SPI" must "have worked" in {

    serviceTry.isSuccess mustBe true

  }

/*  
  "QueryServiceProxy SPI" must "have worked" in {

     QueryServiceProxy.getInstance.isSuccess mustBe true
  }
*/

  lazy val service = serviceTry.get

  private val isValidQCReport: QCReport => Boolean = {

    report =>

      (report.creationDate isBefore LocalDateTime.now) &&
      report.patientTotal >= 0 &&
      report.completionStats.map(_.frequency).forall(
        freq =>
          (freq.count   isIn ClosedInterval(0   -> report.patientTotal)) &&
          (freq.percent isIn ClosedInterval(0.0 -> 100.0))
      ) &&
      report.averageDurations.forall(_.duration.value >= 0)

  }

  private val isValidGlobalQCReport: GlobalQCReport => Boolean = {

    report =>
      isValidQCReport(report) &&
      report.constituentReports.forall(isValidQCReport)

  }


  "LocalQCReport" must "be valid" in {

    for {

      result <- service.getLocalQCReport(PeerToPeerRequest(localSite,querier))

      qcReport = result.toOption.value

      totalAsexpected = qcReport.patientTotal mustBe N

      isValid = isValidQCReport(qcReport) mustBe true

    } yield isValid

  }


  "GlobalQCReport" must "be valid" in {

    for {

      result <- service.compileGlobalQCReport

      qcReport = result.toOption.value

      isValid = isValidGlobalQCReport(qcReport) mustBe true

    } yield isValid

  }


  "Getting all Patients" must "return expected value" in {

    for {

      allPatients <- service.patients

      nAsExpected = allPatients.size mustBe N

    } yield nAsExpected

  }


  "PatientTherapies" must "contain all Patients" in {

    for {

      result <- service.compileGlobalPatientTherapies(None)

      data = result.toOption.value.data

    } yield data.size mustBe N

  }


  "PreparedQuery operations" should "have worked" in {
  
    for {

      result <-
        service ! PreparedQuery.Create(
          "Dummy PreparedQuery",
          Parameters.empty.copy(
            diagnoses = Some(List(Coding(ICD10GM("C25"),None)))
          )
        )

      created = result.isRight mustBe true
        
      queries <- service.preparedQueries

    } yield queries.toOption.value must not be (empty)

  }


  "Local Query results and filtering operations" must "be valid" in {

    import extensions._
    import de.bwhc.mtb.dtos.ValueSets._  // For ValueSet[Gender.Value]
    import de.bwhc.mtb.query.api.Mappings._         // For ValueSet[VitalStatus.Value]
    import LevelOfEvidence.Grading.{m1A,m1B}

    val mode   = Coding(Mode.Local,None)
    val params = Parameters.empty

    val filterGender       = Gender.Female
    val filterVitalStatus  = VitalStatus.Alive
    val filterSpecimenType = Specimen.Type.FFPE

    val vitalStatusDisplay  = ValueSet[VitalStatus.Value].displayOf(filterVitalStatus).get
    val genderDisplay       = ValueSet[Gender.Value].displayOf(filterGender).get
    val specimenTypeDisplay = ValueSet[Specimen.Type.Value].displayOf(filterSpecimenType).get

    for {

      result <- service ! Submit(mode,params)

      query = result.onlyRight.value

      summary <- service.resultSummaryOf(query.id).map(_.value)

      queryPatients <- service.patientsFrom(query.id)

      allPatients   <- service.patients

      _ = queryPatients.value.size mustBe allPatients.size

      _ <- service.ngsSummariesFrom(query.id).map(_.value.size mustBe summary.ngsReportCount)

      _ <- service.therapyRecommendationsFrom(query.id).map(_.value.size mustBe summary.therapyRecommendationCount)

      _ <- service.molecularTherapiesFrom(query.id).map(_.value.size mustBe summary.therapyCount)
      

      filterResult <-
        service ! ApplyFilters(
          query.id,
          Some(
            query.filters.patientFilter
              .copy(
                gender = query.filters.patientFilter.gender.selectOnly(Coding(filterGender)),
                vitalStatus = query.filters.patientFilter.vitalStatus.selectOnly(Coding(filterVitalStatus))
              )
          ),
          Some(
            query.filters.ngsSummaryFilter
              .copy(
                specimenType = query.filters.ngsSummaryFilter.specimenType.selectOnly(Coding(filterSpecimenType))
              )
          ),
          Some(
            query.filters.therapyRecommendationFilter.copy(
              levelOfEvidence =
                query.filters.therapyRecommendationFilter.levelOfEvidence
                  .selectOnly(m1A,m1B)
            )
          ),
          None,
        )


      filteredQuery = filterResult.toOption.value

      filteredPatients <- service.patientsFrom(filteredQuery.id)

      filteredNgsSummaries <- service.ngsSummariesFrom(filteredQuery.id)

      filteredRecommendations <- service.therapyRecommendationsFrom(filteredQuery.id)

      vitalStatusAsExpected =
        filteredPatients.value.map(_.vitalStatus) must contain only (vitalStatusDisplay)

      gendersAsExpected =
        filteredPatients.value.map(_.gender) must contain only (genderDisplay)

      specimenTypeAsExpected =
        filteredNgsSummaries.value.map(_.specimenType.toOption.value) must contain only (specimenTypeDisplay)

      levelOfEvidenceAsExpected =
        filteredRecommendations.value.map(_.levelOfEvidence.toOption.value) must contain only (m1A,m1B)

    } yield succeed

  }


  "Query by ICD-10-GM super-category" should "return sub-categories" in {
  
    val mode = Coding(Mode.Local,None)

   //TODO: instead of hard-coding "C25",
   // find dynamic way of getting an ICD-10 super-class for a code
   // that certainly occurs in the in-mem random generated data sets
   
//    val code = "C25.1"
//    val superClass =
//      icd10s.find(_.code.value == code)
//        .flatMap(c => icd10s.find(_.code == c.superClass.get)).get

    val params =
      Parameters.empty.copy(
        diagnoses = Some(List(Coding(ICD10GM("C25"),None)))
      )

    for {

      result <- service ! Submit(mode,params)

      query = result.onlyRight.value

      patients <- service.patientsFrom(query.id)

      patientsNonEmpty = patients must not be empty

    } yield patientsNonEmpty

  }

}
