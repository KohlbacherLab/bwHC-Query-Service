package de.bwhc.mtb.query.impl



import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.util.Logging
import de.bwhc.util.spi._

import de.bwhc.mtb.query.api.{
  extensions,
  Querier,
  Query,
  Snapshot
}

import de.bwhc.mtb.data.entry.dtos.MTBFile



trait QueryCacheProvider extends SPI[QueryCache]

object QueryCache extends SPILoader(classOf[QueryCacheProvider])


trait QueryCache
{

  self =>

  type Snapshots = Iterable[Snapshot[MTBFile]]
  type ResultSet = Iterable[MTBFile]


  def newQueryId: Query.Id


  def +=(
    qr: (Query, Snapshots)
  ): Unit

  def update(
    qr: (Query,Snapshots)
  ): Unit


  def update(
    query: Query
  ): Unit


  def applyFilter(
    id: Query.Id,
    f: Query.Filter
  ): Option[Query]


  def get(
    id: Query.Id,
  ): Option[Query]


  def resultsOf(
    id: Query.Id,
  ): Option[ResultSet]


  def remove(
    id: Query.Id,
  ): Unit

  def -=(
    id: Query.Id,
  ) = remove(id)


}



//class DefaultQueryCache
object DefaultQueryCache
extends QueryCache
with Logging
{

  import java.util.UUID.randomUUID

  import java.time.Instant

  import java.util.concurrent.{Executors,TimeUnit}

  import scala.collection.concurrent._



  private val queries: Map[Query.Id,Query] =
    TrieMap.empty[Query.Id,Query]

  private val resultSets: Map[Query.Id,Snapshots] =
    TrieMap.empty[Query.Id,Snapshots]

//  private val filters: Map[Query.Id,MTBFile => Boolean] =
//    TrieMap.empty[Query.Id,MTBFile => Boolean]

  private val filters: Map[Query.Id,Query.Filter] =
    TrieMap.empty[Query.Id,Query.Filter]



  import scala.language.implicitConversions

  implicit def toPredicate(f: Query.Filter): MTBFile => Boolean = {

    import extensions._
    import de.bwhc.util.data.Interval._

    mtbfile =>

      val pat = mtbfile.patient

      (f.genders contains pat.gender) &&
        (pat.age.getOrElse(-1) isIn f.ageRange)
  }


  //---------------------------------------------------------------------------
  // Scheduled clean-up task of cached data
  //---------------------------------------------------------------------------

  private class CleanupTask extends Runnable with Logging
  {

    override def run: Unit = {

      log.debug("Running clean-up task for timed out query sessions")

      val timedOutQueryIds =
        queries.values
          .filter(_.lastUpdate isBefore Instant.now.minusSeconds(600)) // 10 min timeout limit
          .map(_.id)

      if (!timedOutQueryIds.isEmpty){
         log.info("Timed out query sessions detected, removing them...")
      }

      queries subtractAll timedOutQueryIds
    
      log.debug("Finished running clean-up task for timed out query sessions")

    }

  }

  private val executor = Executors.newSingleThreadScheduledExecutor

  executor.scheduleAtFixedRate(
    new CleanupTask,
    30,
    60,
    TimeUnit.SECONDS
  )

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------


  def newQueryId: Query.Id = Query.Id(randomUUID.toString)
  

  def +=(
    qr: (Query, Snapshots)
  ): Unit = {

    val (query,results) = qr

    queries    += (query.id -> query)
    resultSets += (query.id -> results)
  }

/*
  private def refresh(queryId: Query.Id): Unit = {
    queries.get(query.id)
      .tapEach(q => queries.replace(queryId,q.copy(lastUpdate = Instant.now)))    
  }
*/

  def update(
    query: Query
  ): Unit = {

    for {
      q  <- queries.replace(query.id,query)
    } yield ()
 
  }


  def update(
    qr: (Query,Snapshots)
  ): Unit = {

    val (query,results) = qr

    queries.replace(query.id,query)
    resultSets.replace(query.id,results)
 
  }


  def applyFilter(
    id: Query.Id,
    f: Query.Filter
  ): Option[Query] = {
    get(id)
      .tapEach(_ => filters += (id -> f))
      .headOption
  }

  def get(
    id: Query.Id,
  ): Option[Query] = {
    queries.get(id)
  }


  def resultsOf(
    id: Query.Id,
  ): Option[ResultSet] = {

    for {
      rs       <- resultSets.get(id)
      mtbfiles =  rs.map(_.data)
      f        =  filters.get(id)
      result   =  f.fold(mtbfiles)(mtbfiles.filter(_))
    } yield result

  }


  def remove(
    id: Query.Id
  ): Unit = {

    queries    -= id
    resultSets -= id
    filters    -= id

  }


}
