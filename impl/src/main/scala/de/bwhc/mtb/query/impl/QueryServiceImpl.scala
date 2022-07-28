package de.bwhc.mtb.query.impl



import java.time.{Instant,YearMonth}
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
  OptionT,
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
  Coding,
  Gender,
  ICD10GM,
  Medication,
  TherapyRecommendation,
  LevelOfEvidence,
  MolecularTherapy,
  StartedMolecularTherapy,
  Specimen,
  SomaticNGSReport,
  Variant,
  RECIST,
  ZPM,
  ValueSets
}
import de.bwhc.mtb.data.entry.views.{
  MolecularTherapyView,
  MTBFileView,
  CarePlanView,
  TherapyRecommendationView,
}
import de.bwhc.catalogs.med.MedicationCatalog
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

  private val localSite =
    Option(System.getProperty("bwhc.zpm.site")).map(ZPM(_)).get  //TODO: improve configurability

  private val db =
    LocalDB.getInstance.get

  private val bwHC =
    BwHCConnector.getInstance.get

  private val queryCache =
    QueryCache.getInstance.getOrElse(new DefaultQueryCache)

  implicit val medicationCatalog =
    MedicationCatalog.getInstance.get


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
)(
  implicit val medicationCatalog: MedicationCatalog
)
extends QueryService
with Logging
with FilteringOps
{

  import Json.{prettyPrint,toJson}
  import de.bwhc.util.mapping.syntax._
  import Mappings._


  def formattedJson[T: Writes](t: T) = prettyPrint(toJson(t)) 


  //---------------------------------------------------------------------------
  // Data Management Operations
  //---------------------------------------------------------------------------
  override def process(
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


  override def patients(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient]] = {

    log.info(s"Processing request for all Patients")

    for {
      snps <- db.latestSnapshots
      pats = snps.map(_.data.patient)
    } yield pats

  }

  override def mtbFile(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {

    log.info(s"Processing request for latest MTBFile snapshot of Patient ${patId.value}")

    db.latestSnapshot(patId)

  }

  override def mtbFileHistory(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]] = {

    log.info(s"Processing request for MTBFile history of Patient ${patId.value}")

    db.history(patId)

  }


  override def savedQueriesOf(
    querier: Querier
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[SavedQueryInfo]] = {

    //TODO TODO
    ???

  }



  override def retrieveMTBFileSnapshot(
    patId: Patient.Id,
    snpId: Option[Snapshot.Id],
    origin: Option[ZPM]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Option[MTBFileView]]] = {

    import de.bwhc.mtb.data.entry.views.mappings._

    val siteLog =
      origin.filterNot(_ == localSite)
        .map(_.value)
        .getOrElse("local")

    log.info(
      s"""Processing direct request for MTBFile:\nPatient ${patId.value}\nSnapshot: ${snpId.map(_.value).getOrElse("latest")}\nSite: $siteLog\nQuerier: ${querier.value}"""
    )

    for {
      errorOrResult <-
        origin match {
    
          case Some(site) if (site != localSite) =>
            bwHC.execute(
              site,
              PeerToPeerMTBFileRequest(
                localSite,
                querier, 
                patId,
                snpId
              )
            )
    
          case _ =>
            db.snapshot(patId,snpId)
             .map(_.asRight[String])
        }
      
      result = 
        errorOrResult.map(_.map(_.data.mapTo[MTBFileView]))

    } yield result
  }

  //---------------------------------------------------------------------------
  // QCReporting operations
  //---------------------------------------------------------------------------

  override def getLocalQCReportFor(
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


  override def compileGlobalQCReport(
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

  override def process(
    cmd: QueryOps.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,Query]] = {

    import QueryOps.Command._

    cmd match {

      //-----------------------------------------------------------------------
      case Submit(mode,params) => {

        log.info(s"Processing Query submission by Querier ${querier.value}")

        // Validate Query Parameters (ICD-10s, ATC Codes, HGNC symbols)
        ParameterValidation(params) match {

          case Invalid(errors) =>
            Future.successful(errors.leftIor[Query])

          case Valid(validatedParams) => {

            val queryId = queryCache.newQueryId
            
            (
              for {
                results <- IorT(submitQuery(queryId,querier,mode.code,params))

                zpms = results.foldLeft(Set.empty[ZPM])((acc,snp) => acc + snp.data.patient.managingZPM.get)

                query =
                  Query(
                    queryId,
                    querier,
                    Instant.now,
                    mode.withDisplay,
                    validatedParams,
                    DefaultFilters(results.map(_.data)),
                    zpms,
                    Instant.now
                  )

                _ = queryCache += (query -> results)

                _ = log.info(s"New Query session opened: ${queryId.value}")

              } yield query
            )
            .value
            
          }
            
        }
      }


      //-----------------------------------------------------------------------
      case Update(id,mode,params) => {

        log.info(s"Updating Query ${id.value}")

        // Validate Query Parameters (ICD-10s, ATC Codes, HGNC symbols)
        ParameterValidation(params) match {

          case Invalid(errors) =>
            Future.successful(errors.leftIor[Query])

          case Valid(validatedParams) => {

            queryCache.get(id).map {

              oldQuery =>
 
                val updatedQuery =
                  oldQuery.copy(
                    mode = mode.withDisplay,
                    parameters = validatedParams,
                    lastUpdate = Instant.now
                  )

                if (oldQuery.mode.code != updatedQuery.mode.code ||
                    oldQuery.parameters != updatedQuery.parameters)

                  (
                    for {
                      results <- IorT(submitQuery(id,oldQuery.querier,updatedQuery.mode.code,updatedQuery.parameters))
                  
                      query =
                        updatedQuery.copy(
                          filters = DefaultFilters(results.map(_.data)),
                          zpms = results.foldLeft(Set.empty[ZPM])((acc,snp) => acc + snp.data.patient.managingZPM.get)
                        )
                  
                      _ = queryCache.update(query -> results)
                  
                      _ = log.info(s"Query updated: ${query.id.value}")
                  
                    } yield query
                  )
                  .value
                    
                else
                  Future.successful(updatedQuery).andThen {
                    case Success(q) => queryCache.update(q)
                  }
                  .map(_.rightIor[String].toIorNel)

              }

            }
            .getOrElse(Future.successful(s"Invalid Query ID ${id.value}".leftIor[Query].toIorNel))

          }
            
      }


      //-----------------------------------------------------------------------
      case ApplyFilters(
        id,
        optPatientFilter,
        optNGSFilter,
        optRecommendationFilter,
        optTherapyFilter
      ) => {

        val result =
          for {
            query <- queryCache.get(id)

            updatedFilters =
              patch(
                query.filters
              )(
                optPatientFilter
                  .tapEach(f => log.info(s"Query ${id.value}, applying PatientFilter:\n${prettyPrint(toJson(f))}"))
                  .headOption
                  .map(f => (filters => filters.copy(patientFilter = f))),
                optNGSFilter
                  .tapEach(f => log.info(s"Query ${id.value}, applying NGSSummaryFilter:\n${prettyPrint(toJson(f))}"))
                  .headOption
                  .map(f => (filters => filters.copy(ngsSummaryFilter = f))),
                optRecommendationFilter
                  .tapEach(f => log.info(s"Query ${id.value}, applying TherapyRecommendationFilter:\n${prettyPrint(toJson(f))}"))
                  .headOption
                  .map(f => (filters => filters.copy(therapyRecommendationFilter = f))),
                optTherapyFilter
                  .tapEach(f => log.info(s"Query ${id.value}, applying MolecularTherapyFilter:\n${prettyPrint(toJson(f))}"))
                  .headOption
                  .map(f => (filters => filters.copy(molecularTherapyFilter = f)))
              )

            updatedQuery =
              query.copy(
                filters = updatedFilters,
                lastUpdate = Instant.now
              )

            _ =  queryCache.update(updatedQuery)

          } yield updatedQuery

        Future.successful(
          result.map(_.rightIor[String])
            .getOrElse(s"Invalid Query ID ${id.value}".leftIor[Query])
            .toIorNel
        )
        
      }


      case Reset(id) => {
        //TODO
        ???
      }

      case Save(id,name,description) => {
        //TODO
        ???
      }

      case Reload(id) => {
        //TODO
        ???
      }

      case Delete(id) => {
        //TODO
        ???
      }


    }

  }

  
  private def patch[T](
    t: T
  )(
    patches: Option[T => T]*
  ): T = {
    
    patches.foldLeft(
      t
    )(
      (tpr,patch) => patch.fold(tpr)(p => p(tpr))
    )
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

              case Ior.Both(errs,_) => {
                errs.toList.foreach(e => log.error(s"Query ${id.value}: $e")) 
              }

              case _ => ()
            }
          }
        }
      else
        Future.successful(
          List.empty[Snapshot[MTBFile]].rightIor[String].toIorNel
        )

    val localResults =
      db.findMatching(ParameterProcessor(params))
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


  override def get(
    query: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]] = {
    Future.successful(queryCache get query)
  }

/*
  override def resultSummaryOf(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[ResultSummary]] = {
    Future {
      for {
        mtbFiles <- queryCache.resultsOf(query)
        qc       =  QCReporting.toLocalQCReport(localSite,mtbFiles)
      } yield {
        ResultSummary(
          query,
          qc.patientTotal,
          qc.completionStats 
        )
      }   
    }   
  }
*/

  override def resultSummaryOf(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[ResultSummary]] = {
    Future {
      for {

        patientFilter <- queryCache.get(query).map(_.filters.patientFilter)

        mtbFiles <- queryCache.resultsOf(query).map(_.filter(patientFilter))

        patientCount = mtbFiles.size

        ngsReportCount = mtbFiles.map(_.ngsReports.fold(0)(_.size)).sum

        recommendationCount = mtbFiles.map(_.recommendations.fold(0)(_.size)).sum

        therapyCount = mtbFiles.flatMap(_.molecularTherapies.getOrElse(List.empty).filter(_.history.nonEmpty)).size

        qc =  QCReporting.toLocalQCReport(localSite,mtbFiles)

      } yield {
        ResultSummary(
          query,
          patientCount,
          ngsReportCount,
          recommendationCount,
          therapyCount,
          qc.completionStats 
        )
      }   
    }   
  }


  override def patientsFrom(
    query: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[PatientView]]] = {

    import PatientView._

    Future {
      for {
        patientFilter <-
          queryCache.get(query)
            .map(_.filters.patientFilter)

        results <-
          queryCache.resultsOf(query)

        patViews =
          results.filter(patientFilter)
            .map(
              mtbfile =>
                (mtbfile.patient,mtbfile.diagnoses.getOrElse(List.empty)).mapTo[PatientView]
            )
      } yield patViews
    }

  }


  override def ngsSummariesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[NGSSummary]]] = {

    Future {
      for {

        filters <-
          queryCache.get(query)
            .map(_.filters)

        resultSet <-
          queryCache.resultsOf(query)
            .map(_.filter(filters.patientFilter))

        ngsSummaries =         
          for {
            mtbfile <- resultSet

            specimens =
              mtbfile.specimens.getOrElse(List.empty)

            ngsReports =
              mtbfile.ngsReports.getOrElse(List.empty)

            summaries <-
              ngsReports
                .map(ngs => (ngs -> specimens.find(_.id == ngs.specimen)))
                .filter(filters.ngsSummaryFilter)
                .map(_.mapTo[NGSSummary])

          } yield summaries 

      } yield ngsSummaries
    }

  }


  override def therapyRecommendationsFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[TherapyRecommendationSummary]]] = {

    import de.bwhc.mtb.data.entry.views.mappings._

    Future {
      for {
        filters <-
          queryCache.get(query)
            .map(_.filters)

        resultSet <-
          queryCache.resultsOf(query)
            .map(_.filter(filters.patientFilter))

        summaries =
          for {
            mtbfile <- resultSet

            ecogs =
              mtbfile.ecogStatus.getOrElse(List.empty)
                .filter(_.effectiveDate.isDefined)

            ngsReports =
              mtbfile.ngsReports.getOrElse(List.empty)

            recommendationSummaries <-
              mtbfile.recommendations.getOrElse(List.empty)
                .filter(filters.therapyRecommendationFilter)
                .map { rec =>
                  (
                   rec,
                   mtbfile.diagnoses.getOrElse(List.empty)
                     .find(_.id == rec.diagnosis),
                   rec.issuedOn
                     .flatMap(date => ecogs.maxByOption(_.effectiveDate.get isBefore date)),                 
                   rec.ngsReport
                     .flatMap(
                       ngsRef =>
                         rec.supportingVariants.flatMap(
                           variantRefs =>
                             ngsReports.find(_.id == ngsRef)
                               .map(
                                 _.variants.filter(variant => variantRefs contains variant.id)
                               )
                       )
                     )
                     .getOrElse(List.empty)
                  )
                  .mapTo[TherapyRecommendationSummary]
                }

          } yield recommendationSummaries

      } yield summaries
    }

  }


  override def molecularTherapiesFrom(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Iterable[MolecularTherapySummary]]] = {
  
    import de.bwhc.mtb.data.entry.views.mappings._

    Future {
      for {
        filters <-
          queryCache.get(query)
            .map(_.filters)

        resultSet <-
           queryCache.resultsOf(query) 

        summaries =
          for {
            mtbfile <-
              resultSet.filter(filters.patientFilter)

            recommendations =
              mtbfile.recommendations.getOrElse(List.empty)

            ngsReports =
              mtbfile.ngsReports.getOrElse(List.empty)

            responses = 
              mtbfile.responses.getOrElse(List.empty)
 
            diagnoses = 
              mtbfile.diagnoses.getOrElse(List.empty)

            therapySummaries <-
              mtbfile.molecularTherapies.getOrElse(List.empty)
                .flatMap(_.history.maxByOption(_.recordedOn))
                .map(
                  th =>
                    (th -> responses.filter(_.therapy == th.id).maxByOption(_.effectiveDate))
                )
                .filter(filters.molecularTherapyFilter)
                .map {
                  case (therapy,response) =>

                    val recommendation =
                      recommendations.find(_.id == therapy.basedOn)

                  (
                   therapy,
                   recommendation.flatMap(rec => diagnoses.find(_.id == rec.diagnosis)),
                   recommendation,
                   recommendation.flatMap( rec =>
                     rec.ngsReport.flatMap( ngsRef =>
                       rec.supportingVariants.flatMap( variantRefs =>
                         ngsReports.find(_.id == ngsRef)
                           .map(
                             _.variants.filter(variant => variantRefs contains variant.id)
                           )
                       )
                     )
                   )
                   .getOrElse(List.empty),
                   response
                  ) 
                  .mapTo[MolecularTherapySummary]
                }
 
          } yield therapySummaries

      } yield summaries
    }

  }


  override def mtbFileFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {

    Future {
      for {
        rs      <- queryCache resultsOf query 
        mtbfile <- rs find (_.patient.id == patId)
      } yield mtbfile 
    }

  }


  override def mtbFileViewFrom(
    query: Query.Id,
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFileView]] = {

    import de.bwhc.mtb.data.entry.views.mappings._

    mtbFileFrom(query,patId)
      .map(_.map(_.mapTo[MTBFileView]))
  }


  //---------------------------------------------------------------------------
  // bwHC Peer-to-peer query
  //---------------------------------------------------------------------------

  override def resultsOf(
    query: PeerToPeerQuery
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]] = {

    log.info(s"Processing external peer-to-peer MTBFile Query ${query.id.value} from ${query.origin} \nQuery: \n${formattedJson(query)}") 

    db.findMatching(ParameterProcessor(query.parameters)) andThen {
      case Success(snps) => {
        val resultIds =
          snps.map(snp => ResultIds(snp.data.patient.id,snp.id))

        log.info(s"ResultSet returned for external peer-to-peer MTBFile Query ${query.id.value}:\n${formattedJson(resultIds)}")
      }
    }

  }

  override def process(
    req: PeerToPeerMTBFileRequest
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {

    log.info(s"Processing external MTBFile Snapshot request: \n${formattedJson(req)}") 

    db.snapshot(req.patId,req.snpId)

  }

  //---------------------------------------------------------------------------
  // bwHC Peer Status Operations
  //---------------------------------------------------------------------------

  override def peerStatusReport(
    implicit ec: ExecutionContext
  ): Future[PeerStatusReport] = {

     log.info("Compiling bwHC Peer Status Report")

     bwHC.peerStatusReport

  }


}
