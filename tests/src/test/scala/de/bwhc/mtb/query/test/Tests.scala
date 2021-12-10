package de.bwhc.mtb.query.test


import java.time.LocalDateTime
import java.nio.file.Files.createTempDirectory

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Gender,
  ZPM,
  ValueSet
}

import de.bwhc.util.data.{Interval,ClosedInterval}
import Interval._
import de.bwhc.mtb.query.api._
import Query._
import QueryOps.Command._
import QCReport._

import scala.math.Ordering.Double.TotalOrdering



object Setup
{

  val tmpDir = createTempDirectory("bwHC_query_test").toFile

  tmpDir.deleteOnExit

  val N = 50;

  val localSite = ZPM("Tübingen")
 
  val querier   = Querier("Dummy")

  System.setProperty("bwhc.zpm.site",localSite.value)
  System.setProperty("bwhc.query.data.dir", tmpDir.getAbsolutePath)
  System.setProperty("bwhc.query.data.generate", N.toString)

  lazy val serviceTry = QueryService.getInstance

}


class Tests extends AsyncFlatSpec
{

  import Setup._


  "QueryService SPI" must "have worked" in {

    serviceTry.isSuccess mustBe true

  }


  val service = serviceTry.get


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

      result <- service.getLocalQCReportFor(localSite,querier)

      qcReport = result.toOption.value

      totalAsexpected = qcReport.patientTotal mustBe N

      isValid = isValidQCReport(qcReport) mustBe true

    } yield isValid

  }


  "GlobalQCReport" must "be valid" in {

    for {

      result <- service.compileGlobalQCReport(querier)

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


  import extensions._
  import de.bwhc.mtb.data.entry.dtos.ValueSets._  // For ValueSet[Gender.Value]
  import de.bwhc.mtb.query.api.Mappings._         // For ValueSet[VitalStatus.Value]


  "Local Query results and operations" must "be valid" in {

    val mode   = Coding(Mode.Local,None)
    val params = Parameters.empty

    val filterGender       = Gender.Female
    val filterVitalStatus  = VitalStatus.Alive
    val vitalStatusDisplay = ValueSet[VitalStatus.Value].displayOf(filterVitalStatus).get
    val genderDisplay      = ValueSet[Gender.Value].displayOf(filterGender).get

    for {

      result <- service ! Submit(querier,mode,params)

      query = result.onlyRight.value

      queryPatients <- service.patientsFrom(query.id)

      allPatients   <- service.patients

      nAsExpected = queryPatients.value.size mustBe allPatients.size

      filterResult <- service !
                        ApplyFilter(
                          query.id,
                          query.filter.copy(
                            vitalStatus = Set(filterVitalStatus),
                            genders = Set(filterGender)
                          )
                        )

      filteredQuery = filterResult.toOption.value

      filteredPatients <- service.patientsFrom(filteredQuery.id)

      vitalStatusAsExpected =
        filteredPatients.value.map(_.vitalStatus) must contain only (vitalStatusDisplay)

      gendersAsExpected =
        filteredPatients.value.map(_.gender) must contain only (genderDisplay)

    } yield succeed

  }


}
