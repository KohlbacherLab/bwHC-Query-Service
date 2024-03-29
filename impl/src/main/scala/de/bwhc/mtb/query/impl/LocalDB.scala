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

import de.bwhc.mtb.dtos.{
  MTBFile,
  Patient
}



trait LocalDBProvider extends SPI[LocalDB]

object LocalDB extends SPILoader[LocalDBProvider]


trait LocalDB
{

  def save(
    mtbFile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Snapshot[MTBFile]]


  def latestSnapshots(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]]


  def latestSnapshot(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]]


  def snapshot(
    patId: Patient.Id,
    snpId: Option[Snapshot.Id]
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
  ): Future[Option[Snapshot[MTBFile]]]

}
