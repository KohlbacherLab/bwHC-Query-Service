package de.bwhc.mtb.query.impl



import java.time.{Instant,YearMonth,LocalDateTime}
import scala.util.{Either,Success,Failure}
import scala.concurrent.{
  Future,
  ExecutionContext
}
import play.api.libs.json.{Json,Writes}
import cats.data.{
  Ior,
  IorNel,
  EitherNel,
  IorT,
  OptionT,
  NonEmptyList
}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.validated._
import cats.syntax.ior._
import cats.instances.future._
import cats.instances.list._
import de.bwhc.util.Logging
import de.bwhc.util.data.ClosedInterval
import de.bwhc.mtb.query.api._
import de.bwhc.mtb.query.api.ReportingAliases._
import de.bwhc.mtb.dtos.{
  MTBFile,
  Patient,
  Consent,
  Coding,
  Gender,
  ICD10GM,
  Medication,
  TherapyRecommendation,
  LevelOfEvidence,
  MolecularTherapy,
  Specimen,
  SomaticNGSReport,
  Variant,
  SimpleVariant,
  CNV,
  DNAFusion,
  RNAFusion,
  RECIST,
  ZPM,
  ValueSets
}
import de.bwhc.mtb.views.{
  MolecularTherapyView,
  MTBFileView,
  CarePlanView,
  TherapyRecommendationView,
  SimpleVariantView,
  CNVView,
  DNAFusionView,
  RNAFusionView
}
import de.bwhc.catalogs.med.MedicationCatalog
import de.bwhc.catalogs.icd.ICD10GMCatalogs
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

  implicit val icd10Catalogs =
    ICD10GMCatalogs.getInstance.get

  private val queryDB =
    QueryDB.getInstance.get


  val instance =
    new QueryServiceImpl(
      localSite,
      db,
      bwHC,
      queryCache,
      queryDB
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
  private val queryCache: QueryCache,
  private val queryDB: QueryDB
)(
  implicit val medicationCatalog: MedicationCatalog,
  implicit val icd10Catalogs: ICD10GMCatalogs
)
extends QueryService
with Logging
with FilteringOps
{

  import Json.{prettyPrint,toJson}
  import de.bwhc.util.mapping.syntax._
  import Mappings._


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

  private def formattedJson[T: Writes](t: T) = prettyPrint(toJson(t)) 


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

    import de.bwhc.mtb.views.mappings._

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
            bwHC.requestMTBFile(
              site,
              PeerToPeerRequest(
                localSite,
                querier,
                MTBFileParameters(
                  patId,
                  snpId
                )
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

  override def getLocalQCReport(
    request: PeerToPeerRequest[Map[String,String]]
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,LocalQCReport]] = {

    log.info(
      s"Processing request for LocalQCReport by Querier ${request.querier.value} from ZPM ${request.origin.value}"
    )

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
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalQCReport]] = {

    log.info(s"Processing request for GlobalQCReport by Querier ${querier.value}")

    val request = PeerToPeerRequest(localSite,querier)

    val extReports =
      bwHC.requestQCReports(request)

    val localReport =
      getLocalQCReport(request)
        .map(Ior.fromEither)
        .map(_.map(List(_)))
        .map(_.toIorNel)

    (localReport,extReports)
      .mapN(_ combine _)
      .map(_.map(QCReporting.toGlobalQCReport))

  }


  //---------------------------------------------------------------------------
  // Therapy Reporting operations
  //---------------------------------------------------------------------------
 
  import TherapyReportingOperations._


  override def compileLocalMedicationDistributionFor(
    request: PeerToPeerRequest[Report.Filters] 
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],LocalDistributionReport[Medication.Coding]]] = {

    val PeerToPeerRequest(origin,querier,_,_) = request

    log.info(s"Compiling Local Medication Distribution Report for Querier: ${querier.value}, Origin: ${origin.value}")

    db.latestSnapshots
      .map(
        snapshots =>
          toLocalMedicationDistributionReport(
            localSite,
            snapshots.map(_.data)
          )
          .rightNel[String]
      )
      .recover {
        case t =>
          t.getMessage.leftNel[LocalDistributionReport[Medication.Coding]]
      }  
  }


  override def compileGlobalMedicationDistribution(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalDistributionReport[Medication.Coding]]] = {
    
    import DistributionReportOperations._

    log.info(s"Compiling Global Medication Distribution Report for Querier: ${querier.value}")

    val request =
      PeerToPeerRequest(localSite,querier,Report.Filters.empty)

    val externalReports =
      bwHC.requestMedicationDistributionReports(request)

    val localReport =
      compileLocalMedicationDistributionFor(request)

    (
      localReport, 
      externalReports
    )
    .mapN(_.map(List(_)).toIor combine _) // Put localReport result into a List within an IorNel, to use 'IorNel.combine'
    .map(
      _.map(_.combineToGlobalReport)
    )

  }


  override def compileLocalTumorEntityDistributionFor(
    request: PeerToPeerRequest[Report.Filters] 
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],LocalDistributionReport[Coding[ICD10GM]]]] = {

    val medication = request.body.medication

    log.info(
      s"""Compiling Local Tumor Entity Distribution Report for Querier: ${request.querier.value}, Origin: ${request.origin.value}.
      Medication filter: ${medication.map(_.code.value).getOrElse("-")}"""
    )
    
    db.latestSnapshots
      .map(
        snapshots =>
          toLocalTumorEntityDistributionReport(
            localSite,
            snapshots.map(_.data)
          )(
            request.body
          )
          .rightNel[String]
      )
      .recover {
        case t =>
          log.error("Problem compiling local Tumor Entity Distribution Report", t)
          t.getMessage.leftNel[LocalDistributionReport[Coding[ICD10GM]]]
      }  

  }

  override def compileGlobalTumorEntityDistribution(
    medication: Option[Medication.Coding]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalDistributionReport[Coding[ICD10GM]]]] = {
    
    import DistributionReportOperations._
    import ParameterValidation.medicationCodingValidator
    import de.bwhc.util.data.Validation._


    log.info(
s"""Compiling Global Tumor Entity Distribution Report for Querier: ${querier.value}.
Medication filter: ${medication.map(_.code.value).getOrElse("-")}"""
    )

    ifDefined(medication){validate(_)} match {

      case Invalid(err) =>
        Future.successful(err.leftIor[GlobalDistributionReport[Coding[ICD10GM]]])

      case Valid(_) =>

        val request =
          PeerToPeerRequest(localSite,querier,Report.Filters(medication))
        
        val externalReports =
          bwHC.requestTumorEntityDistributionReports(request)
        
        val localReport =
          compileLocalTumorEntityDistributionFor(request)
        
        (
          localReport, 
          externalReports
        )
        .mapN(_.map(List(_)).toIor combine _) // Put localReport result into a List within an IorNel, to use 'IorNel.combine'
        .map(
          _.map(_.combineToGlobalReport)
        )

    }

  }


  override def compilePatientTherapies(
    request: PeerToPeerRequest[Report.Filters] 
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],LocalReport[Seq[PatientTherapies]]]] = {

    val parameters =
      Query.Parameters.empty.copy(
        medicationsWithUsage =
          request.body.medication.map(
           coding =>
             Query.MedicationWithUsage(
               Coding(
                 coding.code,
                 None,
                 coding.version
               ),
               Set(Coding(Query.DrugUsage.Used))
             )
          )
          .map(List(_))
      )

    db.findMatching(ParameterProcessor(parameters))
      .map(_.map(_.data))
      .map(toPatientTherapies(localSite,_)(request.body))
      .map(_.rightNel[String])
      .recover {
        case t => t.getMessage.leftNel[LocalReport[Seq[PatientTherapies]]]
      }
  }

  override def compileGlobalPatientTherapies(
    medication: Option[Medication.Coding]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalReport[Seq[PatientTherapies]]]] = {

    import DistributionReportOperations._
    import ParameterValidation.medicationCodingValidator
    import de.bwhc.util.data.Validation._


    log.info(
      s"""Compiling Patient Therapy data for Querier: ${querier.value}.
      Medication filter: ${medication.map(_.code.value).getOrElse("-")}"""
    )

    ifDefined(medication){validate(_)} match {

      case Invalid(err) =>
        Future.successful(err.leftIor[GlobalReport[Seq[PatientTherapies]]])

      case Valid(_) =>

        val request =
           PeerToPeerRequest(localSite,querier,Report.Filters(medication))
        
        val externalData =
          bwHC.requestPatientTherapies(request)
        
        val localData =
          compilePatientTherapies(request)
        
        (
          localData,
          externalData
        )
        .mapN(_.toIor.map(List(_)) combine _) // Put local data into a List within an IorNel, to use 'IorNel.combine'
        .map(
          _.map(
            parts =>
              GlobalReport(
                LocalDateTime.now,
                parts.map(_.site).toList,
                parts.head.filters,   // ensure one of the filters with completed medication coding is used
                parts.flatMap(_.data),
                None
              )
          )
        )
    }
  }


  //---------------------------------------------------------------------------
  // PreparedQuery Operations
  //---------------------------------------------------------------------------
  
  override def process(
    cmd: PreparedQuery.Command
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[EitherNel[String,PreparedQuery]] = {

    import scala.util.chaining._
    import PreparedQuery.{Create,Update,Delete}

    cmd match {

      case Create(name,params) =>

        log.info(s"Storing new PreparedQuery")

        ParameterValidation(params) match {

          case Valid(parameters) =>
            queryDB.save(
              PreparedQuery(
                queryDB.preparedQueryId,
                querier,
                LocalDateTime.now,
                name,
                parameters,
                Instant.now
              )
            )
            .map(_.rightNel[String])
            .recover { 
              case t =>
                s"Failed to save PreparedQuery"
                  .tap(log.error(_,t))
                  .pipe(_.leftNel[PreparedQuery])
            }

          case Invalid(errors) =>
            Future.successful(errors.asLeft[PreparedQuery])
        }

      case Update(id,optName,optParameters) =>
        log.info(s"Updating PreparedQuery ${id.value}")

        queryDB.preparedQuery(id)
          .flatMap {
            case Some(query) =>

              optParameters.map(ParameterValidation)
                .map(_.map(Some(_)))
                .getOrElse(None.validNel[String]) match {

                case Valid(parameters) =>
                  queryDB.save(
                    patch(query)(
                      optName.map(n => (pq => pq.copy(name = n))),
                      optParameters.map(p => (pq => pq.copy(parameters = p))),
                    )
                    .copy(lastUpdate = Instant.now)
                  )
                  .map(_.rightNel[String])
                  .recover { 
                    case t =>
                      s"Failed to update PreparedQuery ${id.value}"
                        .tap(log.error(_,t))
                        .pipe(_.leftNel[PreparedQuery])
                  }

                case Invalid(errors) =>
                  Future.successful(errors.asLeft[PreparedQuery])

              }

            case None =>
              Future.successful(s"Invalid PreparedQuery ID ${id.value}".leftNel[PreparedQuery])
          }


      case Delete(id) =>
        log.info(s"Deleting PreparedQuery ${id.value}")
        queryDB.delete(id)
          .map(_.toRight(s"Invalid PreparedQuery ID ${id.value}").toEitherNel)

    }

  }


  override def preparedQueries(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Seq[PreparedQuery]]] = {

    log.info(s"Loading Prepared Queries of $querier")

    queryDB.preparedQueries(querier)
      .map(_.asRight[String])
      .recover {
        case t => t.getMessage.asLeft[Seq[PreparedQuery]]
      }
  }


  override def preparedQuery(
    id: PreparedQuery.Id,
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[Either[String,Option[PreparedQuery]]] = {

    log.info(s"Loading Prepared Query ${id.value} for $querier")

    queryDB.preparedQuery(id)
      .map(_.asRight[String])
      .recover {
        case t => t.getMessage.asLeft[Option[PreparedQuery]]
      }
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
                    DefaultFilters(results.map(_.data),params),
                    zpms,
                    queryCache.cachingSeconds,
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
                          filters =
                            DefaultFilters(
                              results.map(_.data),
                              updatedQuery.parameters
                            ),
                          zpms =
                            results.foldLeft(Set.empty[ZPM])((acc,snp) => acc + snp.data.patient.managingZPM.get),
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
        bwHC.executeQuery(PeerToPeerRequest(localSite,querier,params)) andThen {
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
        .map(_.filter(_.data.consent.status == Consent.Status.Active))
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
    queryId: Query.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Query]] = {
    Future.successful(
      queryCache.get(queryId)
    )
  }


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


  override def variantsOfInterestOf(
    query: Query.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[VariantsOfInterest]] = {

    import VariantFilteringOps._
    import de.bwhc.mtb.views.mappings._

    Future {
      for {
        parameters <-
          queryCache.get(query).map(_.parameters)

        mtbfiles <- 
          queryCache.resultsOf(query)

        ngsReports =
          mtbfiles.flatMap(_.ngsReports.getOrElse(List.empty))

      } yield {
        VariantsOfInterest(
          query,
          ngsReports
            .flatMap(
              ngs =>
                ngs.simpleVariants.getOrElse(List.empty)
                  .map(_.copy(patient = Some(ngs.patient)))
            )
            .withFilter(SnvOfInterest(parameters.simpleVariants,parameters.mutatedGenes))
            .map(_.mapTo[SimpleVariantView]),
          ngsReports
            .flatMap(
              ngs => 
                ngs.copyNumberVariants.getOrElse(List.empty)
                  .map(_.copy(patient = Some(ngs.patient)))
            )
            .withFilter(CnvOfInterest(parameters.copyNumberVariants,parameters.mutatedGenes))
            .map(_.mapTo[CNVView]),
          ngsReports
            .flatMap(
              ngs =>
                ngs.dnaFusions.getOrElse(List.empty)
                  .map(_.copy(patient = Some(ngs.patient)))
            )
            .withFilter(DnaFusionOfInterest(parameters.dnaFusions,parameters.mutatedGenes))
            .map(_.mapTo[DNAFusionView]),
          ngsReports
            .flatMap(
              ngs =>
                ngs.rnaFusions.getOrElse(List.empty)
                  .map(_.copy(patient = Some(ngs.patient)))
            )
            .withFilter(RnaFusionOfInterest(parameters.rnaFusions,parameters.mutatedGenes))
            .map(_.mapTo[RNAFusionView]),
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

    import de.bwhc.mtb.views.mappings._

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
  
    import de.bwhc.mtb.views.mappings._

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

    import de.bwhc.mtb.views.mappings._

    mtbFileFrom(query,patId)
      .map(_.map(_.mapTo[MTBFileView]))
  }


  //---------------------------------------------------------------------------
  // bwHC Peer-to-peer query
  //---------------------------------------------------------------------------

  override def resultsOf(
    query: PeerToPeerRequest[Query.Parameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]] = {

    log.info(s"Processing external peer-to-peer MTBFile Query from ${query.origin} \nQuery: \n${formattedJson(query)}") 

    db.findMatching(ParameterProcessor(query.body)) andThen {
      case Success(snps) => {
        val resultIds =
          snps.map(snp => ResultIds(snp.data.patient.id,snp.id))

        log.info(s"ResultSet returned for external peer-to-peer MTBFile Query:\n${formattedJson(resultIds)}")
      }
    }

  }

  override def process(
//    req: PeerToPeerMTBFileRequest
    req: PeerToPeerRequest[MTBFileParameters]
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {

    log.info(s"Processing external MTBFile Snapshot request: \n${formattedJson(req)}") 

    db.snapshot(req.body.patId,req.body.snpId)

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
