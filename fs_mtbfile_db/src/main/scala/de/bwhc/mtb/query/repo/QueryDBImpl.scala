package de.bwhc.mtb.query.repo


import java.io.File
import java.util.UUID.randomUUID
import scala.concurrent.{
  Future,
  ExecutionContext
}
import de.ekut.tbi.repo.AsyncRepository
import de.ekut.tbi.repo.fs.AsyncFSBackedInMemRepository
import de.bwhc.mtb.query.api.{
  Querier,
  PreparedQuery
}
import de.bwhc.mtb.query.impl.{
  QueryDB,
  QueryDBProvider
}


class QueryDBProviderImpl extends QueryDBProvider
{

  override def getInstance: QueryDB =
    QueryDBImpl.instance

}


object QueryDBImpl
{

  private val dataDir =
    Option(System.getProperty("bwhc.query.data.dir"))
      .map(new File(_))
      .get

  private val db: AsyncRepository[PreparedQuery,PreparedQuery.Id] =
    AsyncFSBackedInMemRepository(
      new File(dataDir,"preparedQueries/"),
      "PreparedQuery",
      _.id,
      _.value
    )

  val instance =
    new QueryDBImpl(db)

}


class QueryDBImpl 
(
  private val db: AsyncRepository[PreparedQuery,PreparedQuery.Id]
)
extends QueryDB
{

  override def preparedQueryId =
    PreparedQuery.Id(randomUUID.toString)


  override def save(
    pq: PreparedQuery
  )(
    implicit ec: ExecutionContext
  ): Future[PreparedQuery] = {

    db.save(pq)
  }


  override def preparedQueries(
    creator: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Seq[PreparedQuery]] =
    db.query(_.creator == creator)
      .map(_.toSeq)



  override def preparedQuery(
    id: PreparedQuery.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[PreparedQuery]] =
    db.get(id)


  override def delete(
    id: PreparedQuery.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[PreparedQuery]] =
    db.delete(id)


}
