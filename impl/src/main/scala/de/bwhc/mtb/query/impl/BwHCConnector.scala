package de.bwhc.mtb.query.impl


import scala.util.Either
import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.IorNel

import de.bwhc.util.spi._
import de.bwhc.mtb.query.api._

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  ZPM
}


trait BwHCConnectorProvider extends SPI[BwHCConnector]

object BwHCConnector extends SPILoader(classOf[BwHCConnectorProvider])


trait BwHCConnector
{

  def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport]


  def requestQCReports(
    origin: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[LocalQCReport]]]


  def execute(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,List[Snapshot[MTBFile]]]]


  def execute(
    site: ZPM,
    req: PeerToPeerMTBFileRequest
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,Option[Snapshot[MTBFile]]]]


}

