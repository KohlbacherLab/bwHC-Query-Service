package de.bwhc.mtb.query.impl



import java.time.Instant

import scala.util.{Either,Success}
import scala.concurrent.{
  Future,
  ExecutionContext
}

import play.api.libs.json.{Json,Writes}

import cats.data.{
  Ior,
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
import de.bwhc.util.data.ClosedInterval

import de.bwhc.mtb.query.api._

import de.bwhc.mtb.data.entry.dtos.{
  MTBFile,
  Patient,
  ICD10GM,
  Medication,
  TherapyRecommendation,
  MolecularTherapyView,
  MolecularTherapyDocumentation,
  Specimen,
  SomaticNGSReport,
  NGSSummary,
  Variant,
  ZPM
}


import cats.data.Validated.{
  Valid, Invalid, validNel
}



class QueryServiceProviderImpl extends QueryServiceProvider
{

  def getInstance: QueryService = {
    QueryServiceImpl.instance
  }

}


object QueryServiceImpl
{

  private val localSite  = Option(System.getProperty("bwhc.zpm.site")).map(ZPM(_)).get  //TODO: improve configurability
  private val db         = LocalDB.getInstance.get
  private val bwHC       = BwHCConnector.getInstance.get
  private val queryCache: QueryCache = DefaultQueryCache

  val instance =
    new QueryServiceImpl(
      localSite,
      db,
      bwHC,
      queryCache
    )

}


case class ResultIds
(
  patient: Patient.Id,
  snapshot: Snapshot.Id
)
object ResultIds
{
  implicit val format = Json.format[ResultIds]
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

  import Json.{prettyPrint,toJson}


  def formattedJson[T: Writes](t: T) = prettyPrint(toJson(t)) 


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

        log.info(s"Processing new MTBFile import for Patient ${mtbfile.patient.id.value}")

        db.save(mtbfile)
          .map(Created(mtbfile.patient.id,_))
          .map(_.asRight[String])
          .recover {
             case t => t.getMessage.asLeft[DataOps.Response]
          }

      }

      //-----------------------------------------------------------------------
      case Delete(patId) => {

        log.info(s"Processing data deletion request for Patient ${patId.value}")

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

    log.info(s"Processing request for all Patients")

    for {
      snps <- db.latestSnapshots
      pats = snps.map(_.data.patient)
    } yield pats

  }

  def mtbFile(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {

    log.info(s"Processing request for latest MTBFile snapshot of Patient ${patId.value}")

    db.latestSnapshot(patId)

  }

  def mtbFileHistory(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]] = {

    log.info(s"Processing request for MTBFile history of Patient ${patId.value}")

    db.history(patId)

  }


  //---------------------------------------------------------------------------
  // QCReporting operations
  //---------------------------------------------------------------------------

  def getLocalQCReportFor(
    site: ZPM,
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,LocalQCReport]] = {

    log.info(s"Processing request for LocalQCReport by Querier ${querier.value} from ZPM ${site.value}")

    val result =
      for {
        snapshots <- db.latestSnapshots
        mtbFiles  =  snapshots.map(_.data)
        report    =  QCReporting.toLocalQCReport(localSite,mtbFiles)
       } yield report.asRight[String]

    result.recover {
      case t => t.getMessage.asLeft[LocalQCReport]
    }

  }


  def compileGlobalQCReport(
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[IorNel[String,GlobalQCReport]] = {

    log.info(s"Processing request for GlobalQCReport by Querier ${querier.value}")

    val extReports =
      bwHC.requestQCReports(localSite,querier)

    val localReport =
      getLocalQCReportFor(localSite,querier)
        .map(Ior.fromEither)
        .map(_.map(List(_)))
        .map(_.toIorNel)

    (localReport,extReports)
      .mapN(_ combine _)
      .map(_.map(QCReporting.toGlobalQCReport))

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

        log.info(s"Processing Query submission by Querier ${querier.value}")

        // Validate Query Parameters (ICD-10s, ATC Codes, HGNC symbols)
        ParameterValidation(params) match {

          case Invalid(errors) =>
            Future.successful(errors.leftIor[Query])

          case Valid(_) => {

            val queryId = queryCache.newQueryId
            
            val processed =
              for {
                results <- IorT(submitQuery(queryId,querier,mode,params))
                query =
                  Query(
                    queryId,
                    querier,
                    Instant.now,
                    mode,
                    params,
                    defaultFilterOn(results.map(_.data)),
                    Instant.now
                  )

                _ = queryCache += (query -> results)

                _ = log.info(s"New Query session opened: ${queryId.value}")

              } yield query
            
            processed.value
            
          }
            
        }
      }


      //-----------------------------------------------------------------------
      case Update(id,mode,params,filter) => {

        log.info(s"Updating Query ${id.value}")

        val updatedQuery =
          (queryCache get id) match {

            case Some(q) if (q.mode != mode || q.parameters != params) => {

              // Validate Query Parameters (ICD-10s, ATC Codes, HGNC symbols)
              ParameterValidation(params) match {
              
                case Invalid(errors) =>
                  Future.successful(errors.leftIor[Query])
              
                case Valid(_) => {            
                  (for {
                    results <- IorT(submitQuery(id,q.querier,mode,params))
                    up = q.copy(
                           mode = mode,
                           parameters = params,
                           lastUpdate = Instant.now
                         )
                    _ =  queryCache.update(up -> results)
                  } yield up).value

                }

              }
            }

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

        log.info(s"Applying Filter to Query ${id.value}:\n ${prettyPrint(toJson(filter))}")

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

    log.info(s"Submitting Query: ${id.value} \nMode: $mode \nParameters:\n${prettyPrint(toJson(params))}")

    val externalResults =
      if (mode == Query.Mode.Federated)
        bwHC execute PeerToPeerQuery(id,localSite,querier,params) andThen {
          case Success(ior) => {
            ior match {

              case Ior.Left(errs) =>
                errs.toList.foreach(e => log.error(s"In Query ${id.value}: $e")) 

              case Ior.Right(snps) => {
                val resultIds = snps.map(snp => ResultIds(snp.data.patient.id,snp.id))
                log.info(s"External ResultSet for MTBFile Query ${id.value}:\n${formattedJson(resultIds)}")
              }

              case Ior.Both(errs,snps) => {
                errs.toList.foreach(e => log.error(s"Query ${id.value}: $e")) 
                val resultIds = snps.map(snp => ResultIds(snp.data.patient.id,snp.id))
                log.info(s"External ResultSet for MTBFile Query ${id.value}:\n${formattedJson(resultIds)}")
              }
            }
          }
        }
      else
        Future.successful(List.empty[Snapshot[MTBFile]].rightIor[String].toIorNel)

    val localResults =
      db.findMatching(params)
        .map(_.toList)
        .andThen {
          case Success(snps) => {
            val resultIds =
              snps.map(snp => ResultIds(snp.data.patient.id,snp.id))
        
            log.info(s"Local ResultSet for MTBFile Query ${id.value}:\n${formattedJson(resultIds)}")
          }
        }
        .map(_.rightIor[String].toIorNel)
        .recover {
          case t => t.getMessage.leftIor[List[Snapshot[MTBFile]]].toIorNel
        }

    (localResults,externalResults).mapN(_ combine _)

  }


  private def defaultFilterOn(
    mtbfiles: Iterable[MTBFile]
  ): Query.Filter = {

    import extensions._

    val patients = mtbfiles.map(_.patient)

    val genders = patients.map(_.gender).toSet

    val ages = patients.map(_.age).filter(_.isDefined).map(_.get)
    val ageRange = ClosedInterval(ages.minOption.getOrElse(0),ages.maxOption.getOrElse(0))

    val vitalStatus = patients.map(_.vitalStatus).toSet

    Query.Filter(genders, ageRange, vitalStatus)

  }


  def get(
    query: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]] = {
    Future.successful(queryCache get query)
  }


  def resultsOf(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]] = {

    log.info(s"Processing external peer-to-peer MTBFile Query ${query.id.value} from ${query.origin} \nQuery: \n${formattedJson(query)}") 

    db.findMatching(query.parameters) andThen {
      case Success(snps) => {
        val resultIds =
          snps.map(snp => ResultIds(snp.data.patient.id,snp.id))

        log.info(s"ResultSet returned for external peer-to-peer MTBFile Query ${query.id.value}:\n${formattedJson(resultIds)}")
      }
    }

  }



  import de.bwhc.util.mapping.syntax._


  def patientsFrom(
    query: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[PatientView]]] = {

    import PatientView._

    Future.successful(
      for {
        rs   <- queryCache resultsOf query 
        pats =  rs.map(_.patient.mapTo[PatientView])
      } yield pats
    )


  }


  def mtbFileFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {

    Future.successful(
      for {
        rs      <- queryCache resultsOf query 
        mtbfile <- rs find (_.patient.id == patId)
      } yield mtbfile 
    )

  }


  def therapyRecommendationsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[TherapyRecommendation]]] = {

    Future.successful(
      for {
        resultSet <- queryCache.resultsOf(query)
        allRecs =
          for {
            mtbfile <- resultSet
            if mtbfile.recommendations.isDefined
            recs <- mtbfile.recommendations.get
          } yield recs
      } yield allRecs
    )

  }


  def ngsSummariesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[NGSSummary]]] = {

    import NGSSummary._

    Future.successful(
      for {
        resultSet <- queryCache.resultsOf(query)

        ngsSummaries =
          for {
            mtbfile   <- resultSet

            if mtbfile.specimens.isDefined

            specimens =  mtbfile.specimens.get

            if mtbfile.ngsReports.isDefined

            ngs       <- mtbfile.ngsReports.get

                               // look-up garanteed to work by referential integrity check upon import
            specimenWithReport = (specimens.find(_.id == ngs.specimen).get -> ngs)

          } yield specimenWithReport.mapTo[NGSSummary]

      } yield ngsSummaries
    )

  }


  def molecularTherapiesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[MolecularTherapyView]]] = {

    Future.successful(
      for {
        resultSet <- queryCache.resultsOf(query) 
        allMolTh =
          for {
            mtbfile <- resultSet

            if (mtbfile.molecularTherapies isDefined)

            molThs  <- mtbfile.molecularTherapies.get
                         .filterNot(_.history.isEmpty)
                         //TODO: sort by history date to pick earliest MolecularTherapy follow-up record
                         .map(_.history.head)
                         .map(_.mapTo[MolecularTherapyView])
          } yield molThs

      } yield allMolTh
    )

  }


}
