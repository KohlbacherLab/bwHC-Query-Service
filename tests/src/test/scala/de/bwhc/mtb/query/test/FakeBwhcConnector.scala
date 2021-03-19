package de.bwhc.mtb.query.test


import scala.concurrent.{ExecutionContext,Future}

import cats.data.{Ior,IorNel}

import de.ekut.tbi.generators.Gen
import de.bwhc.mtb.data.gens._

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  ZPM
}

import de.bwhc.mtb.query.api._
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
    origin: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]] = {

     Future.successful(
       Ior.right(List.empty[LocalQCReport]).toIorNel[String]
     )

  }


  override def execute(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]] = {

     Future.successful(
       Ior.right(List.empty[Snapshot[MTBFile]]).toIorNel[String]
     )

  }


}
