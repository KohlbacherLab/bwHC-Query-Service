package de.bwhc.mtb.query.impl



import scala.concurrent.{
  Future,
  ExecutionContext
}

import de.bwhc.util.spi._

import de.bwhc.mtb.query.api.{
  History,
  Query,
  Snapshot
}

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  Patient
}



trait LocalDB
{

  def save(
    mtbFile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Snapshot[MTBFile]]


  def allLatestSnapshots(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]]


  def latestSnapshot(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]]


  def history(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]]


  def findMatching(
    parameters: Query.Parameters
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]]



  def delete(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[History[MTBFile]]

}

trait LocalDBProvider extends SPI[LocalDB]

object LocalDB extends SPILoader(classOf[LocalDBProvider])
