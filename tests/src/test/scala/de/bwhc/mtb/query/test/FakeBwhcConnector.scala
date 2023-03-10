package de.bwhc.mtb.query.test


import scala.concurrent.{ExecutionContext,Future}
import cats.data.{Ior,IorNel}
import cats.syntax.either._
import cats.syntax.ior._
import de.ekut.tbi.generators.Gen
import de.bwhc.mtb.data.gens._
import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICD10GM,
  Medication,
  MTBFile,
  ZPM
}
import de.bwhc.mtb.query.api._
import ReportingAliases._
import de.bwhc.mtb.query.impl.{
  BwHCConnector,
  BwHCConnectorProvider
}


class FakeBwHCConnectorProvider extends BwHCConnectorProvider
{

  override def getInstance: BwHCConnector = {
    FakeBwHCConnector
  }

}


object FakeBwHCConnector extends BwHCConnector
{


  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] =
    Future.successful(PeerStatusReport(peerStatus = List.empty))


  override def requestQCReports(
    req: PeerToPeerRequest[Map[String,String]]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

     Future.successful(
       Ior.right(List.empty[LocalQCReport]).toIorNel[String]
     )

  }


  override def requestMedicationDistributionReports(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalDistributionReport[Medication.Coding]]]] =
    Future.successful(
      List.empty[LocalDistributionReport[Medication.Coding]].rightIor[String].toIorNel
    )


  override def requestTumorEntityDistributionReports(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalDistributionReport[Coding[ICD10GM]]]]] =
    Future.successful(
      List.empty[LocalDistributionReport[Coding[ICD10GM]]].rightIor[String].toIorNel
    )


  override def requestPatientTherapies(
    p2pRequest: PeerToPeerRequest[Report.Filters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalReport[Seq[PatientTherapies]]]]] =
    Future.successful(
      List.empty[LocalReport[Seq[PatientTherapies]]].rightIor[String].toIorNel
    )




  override def executeQuery(
    query: PeerToPeerRequest[Query.Parameters]
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

     Future.successful(
       Ior.right(List.empty[Snapshot[MTBFile]]).toIorNel[String]
     )

  }

  override def requestMTBFile(
    site: ZPM,
    mfreq: PeerToPeerRequest[MTBFileParameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,Option[Snapshot[MTBFile]]]] = {

     Future.successful(
       None.asRight[String]
     )
  }

}
