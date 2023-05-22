package de.bwhc.mtb.query.impl



import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.util.Logging
import de.bwhc.util.spi._

import de.bwhc.mtb.query.api.{
  extensions,
  VitalStatus,
  Querier,
  Query,
  Snapshot,
  Selection
}

import de.bwhc.mtb.data.entry.dtos.MTBFile



trait QueryCacheProvider extends SPI[QueryCache]

object QueryCache extends SPILoader[QueryCacheProvider]


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



class DefaultQueryCache
extends QueryCache
with Logging
{

  import java.util.UUID.randomUUID

  import java.time.Instant

  import java.util.concurrent.Executors
  import java.util.concurrent.TimeUnit.SECONDS

  import scala.collection.concurrent._



  private val queries: Map[Query.Id,Query] =
    TrieMap.empty[Query.Id,Query]

  private val resultSets: Map[Query.Id,Snapshots] =
    TrieMap.empty[Query.Id,Snapshots]


  //---------------------------------------------------------------------------
  // Scheduled clean-up task of cached data
  //---------------------------------------------------------------------------

  private class CleanupTask extends Runnable with Logging
  {

    override def run: Unit = {

      log.debug("Running clean-up task for timed out Query sessions")

      val timedOutQueryIds =
        queries.values
          .filter(_.lastUpdate isBefore Instant.now.minusSeconds(600)) // 10 min timeout limit
          .map(_.id)

      if (timedOutQueryIds.nonEmpty){
         log.info("Timed out query sessions detected, removing them...")
      }

      timedOutQueryIds.foreach(remove)

      log.debug("Finished running clean-up task for timed out query sessions")

    }

  }

  private val executor = Executors.newSingleThreadScheduledExecutor

  executor.scheduleAtFixedRate(
    new CleanupTask,
    60,
    60,
    SECONDS
  )

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------


  override def newQueryId: Query.Id = Query.Id(randomUUID.toString)
  

  override def +=(
    qr: (Query,Snapshots)
  ): Unit = {

    val (query,results) = qr

    queries    += (query.id -> query)
    resultSets += (query.id -> results)
  }


  override def update(
    query: Query
  ): Unit = {

    for {
      q  <- queries.put(query.id,query)
    } yield ()
 
  }


  override def update(
    qr: (Query,Snapshots)
  ): Unit = {

    val (query,results) = qr

    queries.put(query.id,query)
    resultSets.put(query.id,results)
 
  }


  override def get(
    id: Query.Id,
  ): Option[Query] = {
    queries.get(id)
  }


  override def resultsOf(
    id: Query.Id,
  ): Option[ResultSet] = {
    for {
      rs <- resultSets.get(id)
    } yield rs.map(_.data)

  }


  override def remove(
    id: Query.Id
  ): Unit = {

    queries    -= id
    resultSets -= id

  }


}
