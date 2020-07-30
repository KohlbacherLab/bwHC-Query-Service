package de.bwhc.mtb.query.api



import java.time.LocalDateTime
//import java.time.temporal.ChronoUnit

import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}

import cats.data.IorNel

import play.api.libs.json._

import de.bwhc.mtb.data.entry.dtos.
{ 
  MTBFile,
  ZPM
}



trait QCReportingOps
{

  def getLocalQCReportFor(
    site: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,LocalQCReport]]


  def compileGlobalQCReport(
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,GlobalQCReport]]

}
