package de.bwhc.mtb.query.impl



import java.time.LocalDateTime

import scala.util.{
  Either, Right, Left,
  Success, Failure
}
import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.{
  Ior,
  IorNel
}
import cats.instances.list._

import org.slf4j.{
  Logger,
  LoggerFactory
}

import de.bwhc.util.Logging
import de.bwhc.util.num._

import de.bwhc.mtb.query.api._
import QCReport._
import QCReport.CompletionLevel._
import QCReport.TimeSpan._

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  Patient,
  ZPM
}



trait QCReporting
{

  def toLocalQCReport(
    zpm: ZPM,
    mtbFiles: Iterable[MTBFile]
  ): LocalQCReport


  def toGlobalQCReport(
    reports: Iterable[LocalQCReport]
  ): GlobalQCReport

}


object QCReporting
extends QCReporting
with Logging
{

  import java.time.temporal.ChronoUnit
  import ChronoUnit._


  def toLocalQCReport(
    zpm: ZPM,
    mtbFiles: Iterable[MTBFile]
  ): LocalQCReport = {

    val total = mtbFiles.size

    val mtbFilesWithNGS      = mtbFiles.filter(_.ngsReports.exists(!_.isEmpty))
    val mtbFilesWithCarePlan = mtbFiles.filter(_.recommendations.exists(!_.isEmpty))
    val mtbFilesWithFU       = mtbFiles.filter(_.molecularTherapies.exists(!_.isEmpty))

    val completionStats =
      Seq(
        Sequenced       -> mtbFilesWithNGS.size,
        CarePlanIssued  -> mtbFilesWithCarePlan.size,
        FollowedUp      -> mtbFilesWithFU.size,
        PatientDeceased -> mtbFiles.filter(_.patient.dateOfDeath.isDefined).size
      )
      .map {
        case (lvl,n) =>
          CompletionLevelWithFrequency(
            lvl,
            Frequency(n, (n percentOf total))
          )
      }

    // Average durations
    implicit val tUnit: ChronoUnit = DAYS

    val tToNGS = 
      for {
        mtbfile   <- mtbFilesWithNGS
        referral  =  mtbfile.episode.period.start
        ngs       <- mtbfile.ngsReports.get
        t         =  tUnit.between(referral,ngs.issueDate)
      } yield t


    val tToCarePlan =
      for {
        mtbfile   <- mtbFilesWithCarePlan
        referral  =  mtbfile.episode.period.start
        carePlan  <- mtbfile.carePlans.get
        if (carePlan.issuedOn isDefined)
        t         =  tUnit.between(referral,carePlan.issuedOn.get)
      } yield t


    val tToFU =
      for {
        mtbfile   <- mtbFilesWithFU
        recomms   =  mtbfile.recommendations.get
        molTh     <- mtbfile.molecularTherapies.get.map(_.history.minBy(_.recordedOn))
        rec       =  recomms.find(_.id == molTh.basedOn).get
        if (rec.issuedOn isDefined)
        t         =  tUnit.between(molTh.recordedOn,rec.issuedOn.get)
      } yield t


    val meanDurations =
      Seq(
        ReferralToSequencing -> Duration(mean(tToNGS),tUnit),
        ReferralToCarePlan   -> Duration(mean(tToCarePlan),tUnit),
        CarePlanToFollowUp   -> Duration(mean(tToFU),tUnit)
      )
      .map { case (s,d) => TimeSpanWithDuration(s,d) }
 
    LocalQCReport(
      LocalDateTime.now,
      zpm,
      total,
      completionStats,
      meanDurations
    )    

  }

  def toGlobalQCReport(
    reports: Iterable[LocalQCReport]
  ): GlobalQCReport = {

    val total =
      reports.map(_.patientTotal).sum

    val completionStats =
      toMapSeq(
        reports.map(
          _.completionStats
           .map { case CompletionLevelWithFrequency(lvl,Frequency(n,_)) => (lvl,n) }
           .toMap
        )
      )
      .view
      .mapValues(_.sum)
      .map {
        case (lvl,n) =>
          CompletionLevelWithFrequency(
            lvl,
            Frequency(n, (n percentOf total))
          )
      }
      .toSeq

    val averageDurations =
      toMapSeq(
        reports.map(
          _.averageDurations
           .map { case TimeSpanWithDuration(s,d) => (s,d) }
           .toMap
          )
      )
      .view
      .mapValues(ds => ds.foldLeft(Duration(0,ds.head.unit))(_ + _)/ds.size)
      .map { case (s,d) => TimeSpanWithDuration(s,d) }
      .toSeq


    GlobalQCReport(
      LocalDateTime.now,
      reports.map(_.zpm).toSet,
      total,
      completionStats,
      averageDurations,
      reports.toSeq
    )

  }


  implicit class IntOps(val n: Int) extends AnyVal
  {
    def percentOf(total: Int): Double = {
      if (total > 0)
        ((n.toDouble)/total*100).withDecimals(1)
      else
        0.0
    }
  }


  private def mean[T](
    vs: Iterable[T]
  )(
    implicit num: Numeric[T]
  ): Double = {
    if (!vs.isEmpty)
      (num.toDouble(vs.sum)/vs.size).withDecimals(1)
    else
      0.0
  }


  private def toMapSeq[K,V](
    maps: Iterable[Map[K,V]]
  ): Map[K,Seq[V]] = {

    val keys = maps.foldLeft(Set.empty[K])((acc,m) => acc ++ m.keySet)

    keys.map(
      k => (k,maps.foldLeft(Seq.empty[V])((vs,m) => m.get(k).fold(vs)(vs :+ _)))
    )
    .toMap
  }

}
