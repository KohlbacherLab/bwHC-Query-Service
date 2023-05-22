package de.bwhc.mtb.query.api


import java.time.{
  LocalDateTime,
  Instant
}
import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}
import cats.data.EitherNel
import play.api.libs.json.Json



final case class PreparedQuery
(
  id: PreparedQuery.Id,
  creator: Querier,
  creationDateTime: LocalDateTime,
  name: String,
  parameters: Query.Parameters,
  lastUpdate: Instant = Instant.now
)


object PreparedQuery
{

  final case class Id(value: String) extends AnyVal


  sealed abstract class Command
  final case class Create
  (
    name: String,
    parameters: Query.Parameters
  )
  extends Command

  final case class Update
  (
    id: PreparedQuery.Id,
    name: Option[String],
    parameters: Option[Query.Parameters]
  )
  extends Command

  final case class Delete
  (
    id: PreparedQuery.Id,
  )
  extends Command


  implicit val formatId            = Json.valueFormat[Id]
  implicit val formatCreate        = Json.format[Create]
  implicit val formatUpdate        = Json.format[Update]
  implicit val formatDelete        = Json.format[Delete]
  implicit val formatPreparedQuery = Json.format[PreparedQuery]
}


trait PreparedQueryOps
{
  self =>


  def process(
    cmd: PreparedQuery.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[EitherNel[String,PreparedQuery]]


  final def !(
    cmd: PreparedQuery.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ) = self.process(cmd)


  def preparedQueries(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Seq[PreparedQuery]]]


  def preparedQuery(
    id: PreparedQuery.Id,
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Option[PreparedQuery]]]


}

