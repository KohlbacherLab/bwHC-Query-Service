package de.bwhc.mtb.query.api



import java.time.LocalDateTime

import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}

import cats.data.IorNel

import play.api.libs.json._

import de.bwhc.mtb.data.entry.dtos.{ 
  MTBFile,
  ZPM
}



object QCReport
{ 

  case class Frequency(count: Int, percent: Double)
  object Frequency
  {
    implicit val formatFreq = Json.format[Frequency]
  }


  object CompletionLevel extends Enumeration
  {
    val Sequenced       = Value("Sequenziert")
    val CarePlanIssued  = Value("Mit MTB-Beschluss")
    val FollowedUp      = Value("Mit Follow-up")
    val PatientDeceased = Value("Verstorben")
/*
    val Sequenced       = Value
    val CarePlanIssued  = Value
    val FollowedUp      = Value
    val PatientDeceased = Value
*/
    implicit val format = Json.formatEnum(this)
  }

  
  object TimeSpan extends Enumeration
  {

    val ReferralToSequencing = Value
    val ReferralToCarePlan   = Value
    val CarePlanToFollowUp   = Value

    implicit val format = Json.formatEnum(this)
  }

  case class CompletionLevelWithFrequency
  (
    level: CompletionLevel.Value,
    frequency: Frequency
  )

  case class TimeSpanWithDuration
  (
    timeSpan: TimeSpan.Value,
    duration: Duration
  )

  implicit val formatCompletionLevelWithFrequency =
    Json.format[CompletionLevelWithFrequency]

  implicit val formatTimeSpanWithDuration =
    Json.format[TimeSpanWithDuration]
  
}


import QCReport._


sealed trait QCReport
{
  val creationDate: LocalDateTime
  val patientTotal: Int
  val completionStats: Seq[CompletionLevelWithFrequency]
  val averageDurations: Seq[TimeSpanWithDuration]
}


case class LocalQCReport
(
  creationDate: LocalDateTime,
  zpm: ZPM,
  patientTotal: Int,
  completionStats: Seq[CompletionLevelWithFrequency],
  averageDurations: Seq[TimeSpanWithDuration]
) extends QCReport


object LocalQCReport
{
  implicit val format =
    Json.format[LocalQCReport]
}


case class GlobalQCReport
(
  creationDate: LocalDateTime,
  zpms: Set[ZPM],
  patientTotal: Int,
  completionStats: Seq[CompletionLevelWithFrequency],
  averageDurations: Seq[TimeSpanWithDuration],
  constituentReports: Seq[LocalQCReport]
) extends QCReport


object GlobalQCReport
{
  implicit val format =
    Json.format[GlobalQCReport]
}

