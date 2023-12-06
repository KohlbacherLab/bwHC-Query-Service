package de.bwhc.mtb.query.repo


import java.io.{
  File,
  FileWriter,
  InputStream,
  FileInputStream
}
import java.time.Instant
import java.util.UUID.randomUUID
import scala.util.{
  Failure,
  Success,
  Using
}
import scala.util.chaining._
import scala.concurrent.{
  Future,
  ExecutionContext
}
import scala.collection.concurrent.{
  Map,
  TrieMap
}
import play.api.libs.json.{
  Format,
  Json
}
import de.bwhc.mtb.dtos._
import de.bwhc.mtb.query.api.{
  Snapshot,
  History,
  Query
}
import de.bwhc.mtb.query.impl.{
  LocalDB,
  LocalDBProvider,
  VariantFilteringOps
}
import de.bwhc.catalogs.med.MedicationCatalog
import de.bwhc.mtb.dto.extensions.CodingExtensions._


class FSBackedLocalDBProvider
  extends LocalDBProvider
{

  def getInstance: LocalDB = {
    FSBackedLocalDB.instance
  }

}


object FSBackedLocalDB
{

  private def toFileInputStream(f: File): InputStream =
    new FileInputStream(f)

  private val dataDir =
    Option(System.getProperty("bwhc.query.data.dir")).map(new File(_)).get


  //---------------------------------------------------------------------------
  // To add random-generated MTBFiles in case of empty initial data
  import de.ekut.tbi.generators.Gen
  import de.bwhc.mtb.dto.gens._

  implicit val rnd = new scala.util.Random(42)

  implicit val genSnapshotId: Gen[Snapshot.Id] =
    Gen.uuidStrings.map(Snapshot.Id)

  implicit val genMTBFileSnapshot: Gen[Snapshot[MTBFile]] =
    for {
      id      <- Gen.of[Snapshot.Id]
      mtbfile <- Gen.of[MTBFile]
    } yield Snapshot(id,Instant.now,mtbfile)
  //---------------------------------------------------------------------------


  val instance: FSBackedLocalDB = {

    if (!dataDir.exists) dataDir.mkdirs

    val initData: Map[Patient.Id,Snapshot[MTBFile]] =
      Option {
        dataDir.listFiles(
          (_,name) =>
            (name startsWith "Patient_") && (name endsWith ".json")
        )
        .to(LazyList)
        .map(toFileInputStream)
        .map(Json.parse)
        .map(Json.fromJson[Snapshot[MTBFile]](_))
        .map(_.get)
      }
      //------------------------------------------------------
      // In case there are no REAL imported MTBFiles,
      // compensate with a bit of random-generated in-memory data 
      .filter(_.nonEmpty)
      .orElse {

        for {
          nGen <-
            Option(System.getProperty("bwhc.query.data.generate"))
              .map(_.toInt)
              .filter(_ > 0)

          localSite <-
            Option(System.getProperty("bwhc.zpm.site"))
              .map(ZPM(_))

        } yield LazyList.fill(nGen)(Gen.of[Snapshot[MTBFile]].next)
          .map { snp =>
            val mtbfile = snp.data
            snp.copy(data = mtbfile.copy(patient = mtbfile.patient.copy(managingZPM = Some(localSite))))
          }
      }
      .getOrElse(LazyList.empty)
      //------------------------------------------------------
      .foldLeft(
        TrieMap.empty[Patient.Id,Snapshot[MTBFile]]
      ){
        (acc,snp) =>

          acc.updateWith(snp.data.patient.id){

            case Some(s) =>
              if (s.timestamp isBefore snp.timestamp)
                Some(snp)
              else
                Some(s)

            case None => Some(snp)
          }
          acc

      }

    new FSBackedLocalDB(
      dataDir,
      initData
    )

  }


  import Query._
  import DrugUsage._
  import Gene.HgncId

  implicit val atc =
    MedicationCatalog.getInstance.get


  import scala.language.implicitConversions
  
  implicit def toPredicate(params: Parameters): Snapshot[MTBFile] => Boolean = {

    case Snapshot(_,_,mtbfile) =>

      import VariantFilteringOps._


      def matchesQuery[T](
        vals: Iterable[T],
        selection: List[T]
      ): Boolean = {
        selection.isEmpty || vals.exists(selection.contains)
      }


      lazy val diagnosesSelection            = params.diagnoses.getOrElse(List.empty).map(_.code)
      lazy val morphologySelection           = params.tumorMorphology.getOrElse(List.empty).map(_.code)
      lazy val responsesSelection            = params.responses.getOrElse(List.empty).map(_.code)
      lazy val medicationsWithUsageSelection = params.medicationsWithUsage.getOrElse(List.empty)

/*      
      val (
        anyUsageDrugSelection,
        recommendedDrugSelection,
        usedDrugSelection,
        bothUsagesDrugSelection
      ) =
        medicationsWithUsageSelection.foldLeft(
          (
           List.empty[Medication.Code],
           List.empty[Medication.Code],
           List.empty[Medication.Code],
           List.empty[Medication.Code]
          )
        ){
          case ((any,recomm,used,both),Query.MedicationWithUsage(med,usages)) =>
            usages match {
              case s if (s.exists(_.code == Recommended) && s.exists(_.code == Used)) => (any, recomm, used, med.code :: both) 
              case s if s.exists(_.code == Recommended)                               => (any, med.code :: recomm, used, both) 
              case s if s.exists(_.code == Used)                                      => (any, recomm, med.code :: used, both) 
              case _                                                                  => (med.code :: any, recomm, used, both)
            }
        }


      lazy val recommendedDrugCodes =
        mtbfile.recommendations
          .getOrElse(List.empty)
          .map(_.medication.getOrElse(List.empty).toSet)
          .map(_.map(_.code))
          .fold(Set.empty[Medication.Code])(_ ++ _)

      lazy val usedDrugCodes =
        mtbfile.molecularTherapies
          .getOrElse(List.empty)
          .flatMap(_.history.maxByOption(_.recordedOn))
          .map(_.medication.getOrElse(List.empty).toSet)
          .map(_.map(_.code))
          .fold(Set.empty[Medication.Code])(_ ++ _)
*/

      lazy val recommendedDrugs =
        mtbfile.recommendations
          .getOrElse(List.empty)
          .flatMap(
            _.medication
             .getOrElse(List.empty)
             .flatMap(_.complete.display)
          )
          .map(_.toLowerCase)
          .toSet

      lazy val usedDrugs =
        mtbfile.molecularTherapies
          .getOrElse(List.empty)
          .flatMap(_.history.maxByOption(_.recordedOn))
          .flatMap(
            _.medication
             .getOrElse(List.empty)
             .flatMap(_.complete.display)
          )
          .map(_.toLowerCase)
          .toSet

      val medicationFulfilled =
        params.medicationsWithUsage
          .filter(_.nonEmpty)
          .fold(true)(
            _.exists {
              case Query.MedicationWithUsage(med,usages) =>

                usages.map(_.code).pipe {
                
                  case s if s.contains(Recommended) && s.contains(Used) =>
                    (recommendedDrugs & usedDrugs)
                
                  case s if s.contains(Recommended) =>
                    recommendedDrugs
                
                  case s if s.contains(Used) =>
                    usedDrugs
                
                  case _ => 
                    (recommendedDrugs | usedDrugs) 
                }
                  .exists(name => med.display.exists(name contains _.toLowerCase))

            }
          )


      lazy val mutatedGeneIdSelection =
        params.mutatedGenes.getOrElse(List.empty).map(_.code)

      lazy val mutatedGeneIds =
        for {

          ngs <- mtbfile.ngsReports.getOrElse(List.empty)

          snvGeneIds =
            ngs.simpleVariants.getOrElse(List.empty)
              .flatMap(_.gene.flatMap(_.hgncId).toList)

          cnvGeneIds =
            ngs.copyNumberVariants.getOrElse(List.empty)
              .flatMap(
                _.reportedAffectedGenes.getOrElse(List.empty)
                 .flatMap(_.hgncId.toList)
              )

          dnaFusionGeneIds =
            ngs.dnaFusions.getOrElse(List.empty)
              .flatMap(
                dna =>
                  dna.fusionPartner5prime.flatMap(_.gene.hgncId) ++
                    dna.fusionPartner3prime.flatMap(_.gene.hgncId)
              )

          rnaFusionGeneIds =
            ngs.rnaFusions.getOrElse(List.empty)
              .flatMap(
                dna =>
                  dna.fusionPartner5prime.flatMap(_.gene.hgncId) ++
                    dna.fusionPartner3prime.flatMap(_.gene.hgncId)
              )

          geneIds <-
            snvGeneIds ++
             cnvGeneIds ++
              dnaFusionGeneIds ++
               rnaFusionGeneIds

          //TODO: genes from RNA-Seq??

        } yield geneIds


      lazy val snvsMatch =
        params.simpleVariants
          .filter(_.nonEmpty)
          .fold(true){ snvParams => 

            val snvs =
              mtbfile.ngsReports
                .getOrElse(List.empty)
                .flatMap(_.simpleVariants.getOrElse(List.empty))
          
            // check if any param matches, not all
            snvParams.exists(snvs.exists(_)) 
          }
        
      lazy val cnvsMatch =
        params.copyNumberVariants.filter(_.nonEmpty)
          .fold(true){ cnvParams => 

            val cnvs =
              mtbfile.ngsReports
                .getOrElse(List.empty)
                .flatMap(_.copyNumberVariants.getOrElse(List.empty))

            // check if any param matches, not all
            cnvParams.exists(cnvs.exists(_))
          }

      lazy val dnaFusionsMatch =
        params.dnaFusions.filter(_.nonEmpty)
          .fold(true){ fusionParams => 

            val dnaFusions =
              mtbfile.ngsReports
                .getOrElse(List.empty)
                .flatMap(_.dnaFusions.getOrElse(List.empty))

            // check if any param matches, not all
            fusionParams.exists(dnaFusions.exists(_))
          }

      lazy val rnaFusionsMatch =
        params.rnaFusions.filter(_.nonEmpty)
          .fold(true){ fusionParams => 

            val rnaFusions =
              mtbfile.ngsReports
                .getOrElse(List.empty)
                .flatMap(_.rnaFusions.getOrElse(List.empty))

            // check if any param matches, not all
            fusionParams.exists(rnaFusions.exists(_))
          }

      snvsMatch &&
      cnvsMatch &&
      dnaFusionsMatch &&
      rnaFusionsMatch &&
      matchesQuery(mutatedGeneIds, mutatedGeneIdSelection) &&
      matchesQuery(mtbfile.diagnoses.getOrElse(List.empty).map(_.icd10.get.code), diagnosesSelection) &&
      matchesQuery(mtbfile.histologyReports.getOrElse(List.empty).flatMap(_.tumorMorphology).map(_.value.code), morphologySelection) &&
      matchesQuery(mtbfile.responses.getOrElse(List.empty).map(_.value.code), responsesSelection) &&
      medicationFulfilled
//      matchesQuery(recommendedDrugCodes & usedDrugCodes, bothUsagesDrugSelection) &&
//      matchesQuery(recommendedDrugCodes, recommendedDrugSelection) &&
//      matchesQuery(usedDrugCodes, usedDrugSelection ) &&
//      matchesQuery(recommendedDrugCodes ++ usedDrugCodes, anyUsageDrugSelection)

  }

}


class FSBackedLocalDB private (
  dataDir: File,
  cache: Map[Patient.Id,Snapshot[MTBFile]] 
)(
  implicit jsf: Format[Snapshot[MTBFile]]
) extends LocalDB
{


  private def newSnapshotId: Snapshot.Id =
    Snapshot.Id(randomUUID.toString)


  def save(
    mtbFile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Snapshot[MTBFile]] = {

    val patId = mtbFile.patient.id

    val snp = Snapshot(newSnapshotId,Instant.now,mtbFile)

    Future.fromTry {
      Using(
        new FileWriter(
          new File(
            dataDir,
            s"Patient_${patId.value}_Snapshot_${snp.id.value}.json"
          )
        )
      ){
        _.write(Json.prettyPrint(Json.toJson(snp)))
      }
    }
    .andThen {
      case Success(_) => cache.update(patId,snp)
    }
    .map(_ => snp)

  }


  def latestSnapshot(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {
    Future.successful(
      cache.get(patId)
    )
  }

  def snapshot(
    patId: Patient.Id,
    optSnpId: Option[Snapshot.Id]
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {
    
    optSnpId match {

      case None => latestSnapshot(patId)

      case Some(Snapshot.Id(snp)) =>
        Future {
          dataDir.listFiles(
            (_,name) => 
              (name startsWith s"Patient_${patId.value}") && 
              (name contains s"Snapshot_$snp") && 
              (name endsWith ".json")
        )
        .to(LazyList)
        .headOption
        .map(FSBackedLocalDB.toFileInputStream)
        .map(Json.parse)
        .map(Json.fromJson[Snapshot[MTBFile]](_))
        .map(_.get)
      }
    }
  }

  def history(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]] = {
    Future {
      dataDir.listFiles(
        (_,name) => (name startsWith s"Patient_${patId.value}") && (name endsWith ".json")
      )
      .to(LazyList)
      .map(FSBackedLocalDB.toFileInputStream)
      .map(Json.parse)
      .map(Json.fromJson[Snapshot[MTBFile]](_))
      .map(_.get)
      .toList
    }
    .map(Option(_))
    .map(
      _.filterNot(_.isEmpty)
       .map(History(_))
    )
  }



  def findMatching(
    parameters: Query.Parameters
  )(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]] = {

    import FSBackedLocalDB.toPredicate

    for {
      mtbfiles <- latestSnapshots
      result   =  mtbfiles filter parameters 
    } yield result

  }


  def latestSnapshots(
    implicit ec: ExecutionContext
  ): Future[Iterable[Snapshot[MTBFile]]] = {
    Future.successful(
      cache.values
    )
  }


  def delete(
    patId: Patient.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {
    Future {
      cache.remove(patId)
    } andThen {

      case Success(_) =>
        dataDir.listFiles(
          (_,name) => name startsWith s"Patient_${patId.value}"
        )
        .foreach(_.delete)
    }
  }


}
