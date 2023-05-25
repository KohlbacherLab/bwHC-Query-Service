package de.bwhc.mtb.query.impl


import scala.util.Either
import scala.concurrent.{
  ExecutionContext,
  Future
}
import cats.data.IorNel
import de.bwhc.util.spi._
import de.bwhc.mtb.query.api._
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  MTBFile,
  Medication,
  ZPM
}


trait BwHCConnectorProvider extends SPI[BwHCConnector]

object BwHCConnector extends SPILoader[BwHCConnectorProvider]


trait BwHCConnector
{

  import de.bwhc.mtb.query.api.ReportingAliases._


  def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport]


  def requestQCReports(
    p2pRequest: PeerToPeerRequest[Map[String,String]]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]]


  def requestMedicationDistributionReports(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalDistributionReport[Medication.Coding]]]]


  def requestTumorEntityDistributionReports(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalDistributionReport[Coding[ICD10GM]]]]]


  def requestPatientTherapies(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalReport[Seq[PatientTherapies]]]]]


  def executeQuery(
    query: PeerToPeerRequest[Query.Parameters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]]


  def requestMTBFile(
    site: ZPM,
    p2pRequest: PeerToPeerRequest[MTBFileParameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,Option[Snapshot[MTBFile]]]]


}

