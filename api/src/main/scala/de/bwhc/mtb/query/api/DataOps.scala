package de.bwhc.mtb.query.api


import java.time.Instant


import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}

import cats.data.NonEmptyList

import de.bwhc.util.ddd.Event
import de.bwhc.mtb.data.entry.dtos._


trait DataOps
{

  def process(
    cmd: DataOps.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,DataOps.Response]]

  def !(cmd: DataOps.Command)(implicit ec: ExecutionContext) = process(cmd)


  def patients(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient]]

  def mtbFile(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]]


  def mtbFileHistory(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]]


}

object DataOps
{

  sealed abstract class Command
  object Command
  {

    final case class Upload(mtbfile: MTBFile) extends Command
 
    final case class Delete(patId: Patient.Id) extends Command

  }

  sealed abstract class Response extends Event
  object Response
  {

    final case class Created
    (
      patId: Patient.Id,
      snapshot: Snapshot[MTBFile],
      timestamp: Instant = Instant.now
    ) extends Response
   
    final case class Deleted
    (
      patId: Patient.Id,
      timestamp: Instant = Instant.now
    ) extends Response

  }

}

