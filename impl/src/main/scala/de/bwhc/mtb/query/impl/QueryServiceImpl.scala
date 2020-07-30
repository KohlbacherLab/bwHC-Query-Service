package de.bwhc.mtb.query.impl



import java.time.Instant

import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}

import play.api.libs.json.Json.{
  prettyPrint,
  toJson
}

import cats.data.{
  IorNel,
  IorT,
  NonEmptyList
}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.ior._
import cats.instances.future._
import cats.instances.list._

import de.bwhc.util.Logging
import de.bwhc.util.data.Interval

import de.bwhc.mtb.query.api._

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  Patient,
  ZPM
}



class QueryServiceProviderImpl extends QueryServiceProvider
{

  def getInstance: QueryService = {

    val localSite  = Option(System.getProperty("bwhc.zpm.site")).map(ZPM(_)).get  //TODO: improve configurability
    val db         = LocalDB.getInstance.get //TODO: ensure singleton
    val bwHC       = BwHCConnector.getInstance.get
    val queryCache = QueryCache.getInstance.getOrElse(DefaultQueryCache)

    new QueryServiceImpl(
      localSite,
      db,
      bwHC,
      queryCache
    )

  }

}


class QueryServiceImpl
(
  private val localSite: ZPM,
  private val db: LocalDB,
  private val bwHC: BwHCConnector,
  private val queryCache: QueryCache
)
extends QueryService
with Logging
{


  //---------------------------------------------------------------------------
  // Data Management Operations
  //---------------------------------------------------------------------------
  def process(
    cmd: DataOps.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,DataOps.Response]] = {

    import DataOps.Command._
    import DataOps.Response._

    cmd match {

      //-----------------------------------------------------------------------
      case Upload(mtbfile) => {

        log.info(s"Handling new MTBFile import for Patient ${mtbfile.patient.id.value}")

        db.save(mtbfile)
          .map(Created(mtbfile.patient.id,_))
          .map(_.asRight[String])
          .recover {
             case t => t.getMessage.asLeft[DataOps.Response]
          }

      }

      //-----------------------------------------------------------------------
      case Delete(patId) => {

        log.info(s"Handling data deletion request for Patient ${patId.value}")

        db.delete(patId)
          .map(_ => Deleted(patId))
          .map(_.asRight[String])
          .recover {
             case t => t.getMessage.asLeft[DataOps.Response]
          }

      }

    }

  }


  def patients(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient]] = {

    for {
      snps <- db.allLatestSnapshots
      pats = snps.map(_.data.patient)
    } yield pats

  }

  def mtbFile(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {

    db.latestSnapshot(patId)

  }

  def mtbFileHistory(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]] = {

    db.history(patId)

  }


  //---------------------------------------------------------------------------
  // Query Operations
  //---------------------------------------------------------------------------

  def process(
    cmd: QueryOps.Command
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,Query]] = {

    import QueryOps.Command._

    cmd match {

      //-----------------------------------------------------------------------
      case Submit(querier,mode,params) => {

        log.info(s"Processing new query for Querier ${querier.value}")
        log.trace(s"Query Mode: $mode")
        log.trace(s"Query parameters: $params") //TODO params to JSON

        val queryId = queryCache.newQueryId

        val processed =
          for {
            results <- IorT(submitQuery(queryId,querier,mode,params))
            query   =  Query(
                         queryId,
                         querier,
                         Instant.now,
                         mode,
                         params,
                         defaultFilterOn(results.map(_.data)),
                         Instant.now
                       )
            _       =  queryCache += (query -> results)
          } yield query

        processed.value

      }


      //-----------------------------------------------------------------------
      case Update(id,mode,params,filter) => {

        log.info(s"Updating Query ${id.value}")

        val updatedQuery =
          (queryCache get id) match {

            case Some(q) if (q.mode != mode || q.parameters != params) =>
              (for {
                results <- IorT(submitQuery(id,q.querier,mode,params))
                up = q.copy(
                       mode = mode,
                       parameters = params,
                       lastUpdate = Instant.now
                     )
                _ =  queryCache.update(up -> results)
              } yield up).value

            case Some(q) =>
              Future.successful(q.rightIor[String].toIorNel)

            case None =>
              Future.successful(s"Invalid Query ID ${id.value}".leftIor[Query].toIorNel)
          }

        filter.fold(
          updatedQuery
        )(
          this ! ApplyFilter(id,_)
        )

      }


      //-----------------------------------------------------------------------
      case ApplyFilter(id,filter) => {

        log.info(s"Applying filter to Query ${id.value}")
        log.trace(s"Filter: ${prettyPrint(toJson(filter))}")

        val applied =
          for {
            query   <- (queryCache get id)
            _       =  queryCache.applyFilter(id,filter)
            updated =  query.copy(filter = filter, lastUpdate = Instant.now)
            _       =  queryCache.update(updated)
            _       =  log.trace(s"Applied filter to Query ${id.value}")
          } yield updated

        Future.successful(
          applied.map(_.rightIor[String])
            .getOrElse(s"Invalid Query ID ${id.value}".leftIor[Query])
            .toIorNel
        )

      }
    }

  }



  private def submitQuery(
    id: Query.Id,
    querier: Querier,
    mode: Query.Mode.Value,
    params: Query.Parameters
  )(
    implicit
    ec: ExecutionContext
  ): Future[IorNel[String,Iterable[Snapshot[MTBFile]]]] = {

//TODO: log

    val externalResults =
      if (mode == Query.Mode.Federated)
        bwHC.execute(PeerToPeerQuery(id,localSite,querier,params))
      else
        Future.successful(List.empty[Snapshot[MTBFile]].rightIor[String].toIorNel)

    val localResults =
      db.findMatching(params)
        .map(_.toList)
        .map(_.rightIor[String].toIorNel)
        .recover {
          case t => t.getMessage.leftIor[List[Snapshot[MTBFile]]].toIorNel
        }

     (externalResults,localResults).mapN(_ combine _)

  }


  private def defaultFilterOn(
    mtbfiles: Iterable[MTBFile]
  ): Query.Filter = {

    import extensions._

    val patients = mtbfiles.map(_.patient)

    val genders = patients.map(_.gender).toSet

    val ages = patients.map(_.age).filter(_.isDefined).map(_.get)
    val ageRange = Interval.Closed(ages.minOption.getOrElse(0),ages.maxOption.getOrElse(0))

    val vitalStatus = patients.map(_.vitalStatus).toSet

    Query.Filter(genders, ageRange, vitalStatus)

  }


  def get(
    id: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]] = {

???
  }

  def resultsOf(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[MTBFile]] = {

???

  }


  def patientsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[PatientView]]] = {

???

  }

  def mtbFileFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {

???

  }



}
