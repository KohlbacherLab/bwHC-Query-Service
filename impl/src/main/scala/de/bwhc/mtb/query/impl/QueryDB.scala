package de.bwhc.mtb.query.impl


import java.time.LocalDateTime
import scala.concurrent.{ExecutionContext,Future}
import de.bwhc.util.spi.{SPI,SPILoader}
import de.bwhc.mtb.query.api.{Query,Querier,SavedQueryInfo}


trait QueryDBOps[F[_],Env]
{

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


}

trait QueryDB extends QueryDBOps[Future,ExecutionContext]

trait QueryDBProvider extends SPI[QueryDB]

object QueryDB extends SPILoader(classOf[QueryDBProvider])
