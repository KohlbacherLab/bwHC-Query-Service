package de.bwhc.mtb.query.impl


import java.time.LocalDateTime
import scala.concurrent.{
  ExecutionContext,
  Future
}
import de.bwhc.util.spi.{
  SPI,
  SPILoader
}
import de.bwhc.mtb.query.api.{
  PreparedQuery,
  Querier
}


trait QueryDB
{

  def preparedQueryId: PreparedQuery.Id

  def save(
    pq: PreparedQuery
  )(
    implicit ec: ExecutionContext
  ): Future[PreparedQuery]


  def preparedQueries(
    creator: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Seq[PreparedQuery]]


  def preparedQuery(
    id: PreparedQuery.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[PreparedQuery]]


  def delete(
    id: PreparedQuery.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[PreparedQuery]]



/*
  def save(
    query: SavedQuery
  )(
    implicit env: Env
  ): F[SavedQuery]


  def queriesOf(
    querier: Querier
  )(
    implicit env: Env
  ): F[Seq[SavedQueryInfo]]


  def get(
    id: Query.Id
  )(
    implicit env: Env
  ): F[Option[SavedQuery]]


  def delete(
    id: Query.Id
  )(
    implicit env: Env
  ): F[Option[SavedQuery]]
*/

}

trait QueryDBProvider extends SPI[QueryDB]

object QueryDB extends SPILoader[QueryDBProvider]
