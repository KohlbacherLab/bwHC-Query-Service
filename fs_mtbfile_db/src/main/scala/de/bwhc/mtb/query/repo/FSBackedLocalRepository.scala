package de.bwhc.mtb.query.repo


import java.io.{
  File,
  FileWriter,
  InputStream,
  FileInputStream
}
import java.time.Instant
import java.util.UUID.randomUUID

import scala.util.{Failure,Success,Using}
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

import de.bwhc.mtb.data.entry.dtos._

import de.bwhc.mtb.query.api.{
  Snapshot,
  History,
  Query
}
import de.bwhc.mtb.query.impl.{
  LocalDB,
  LocalDBProvider
}




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

  private val dataDir = Option(System.getProperty("bwhc.query.data.dir")).map(new File(_)).get


  //---------------------------------------------------------------------------
  // To add random-generated MTBFiles in case of empty initial data
  import de.ekut.tbi.generators.Gen
  import de.bwhc.mtb.data.gens._

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

    val initData: Seq[(Patient.Id,Snapshot[MTBFile])] =
      Option {
        dataDir.listFiles(
          (_,name) => name.startsWith("Patient_") && name.endsWith(".json")
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
      .filterNot(_.isEmpty)
      .getOrElse {

        val defaultN  = Option(System.getProperty("bwhc.query.data.generate")).map(_.toInt).getOrElse(0) 

        val localSite = Option(System.getProperty("bwhc.zpm.site")).map(ZPM(_)).get

        LazyList.fill(defaultN)(Gen.of[Snapshot[MTBFile]].next)
          .map { snp =>
            val mtbfile = snp.data
            snp.copy(data = mtbfile.copy(patient = mtbfile.patient.copy(managingZPM = Some(localSite))))
          }
      }
      //------------------------------------------------------
      .groupBy(_.data.patient.id)
      .view
// sort Snapshots in DECREASING order of timestamp, i.e. to have MOST RECENT as head
      .mapValues(_.maxBy(_.timestamp)) // get most recent snapshot
      .toSeq

    new FSBackedLocalDB(
      dataDir,
      TrieMap(initData: _*) 
    )

  }


  import Query._
  import DrugUsage._
  import Gene.HgncId

  import scala.language.implicitConversions

  implicit def toPredicate(params: Parameters): Snapshot[MTBFile] => Boolean = {

    case Snapshot(_,_,mtbfile) =>

      def matchesQuery[T](
        vals: Iterable[T],
        selection: Set[T]
      ): Boolean = {
        selection.isEmpty || vals.exists(selection.contains)
      }

      val diagnosesSelection            = params.diagnoses.getOrElse(Set.empty).map(_.code)
      val morphologySelection           = params.tumorMorphology.getOrElse(Set.empty).map(_.code)
      val mutatedGeneIdSelection        = params.mutatedGenes.getOrElse(Set.empty).map(_.code)
      val responsesSelection            = params.responses.getOrElse(Set.empty).map(_.code)
      val medicationsWithUsageSelection = params.medicationsWithUsage.getOrElse(Set.empty)

      val (usedDrugSel,recDrugSel) = medicationsWithUsageSelection.partition(_.usage.code == Used)

      val recommendedDrugCodes =
        mtbfile.recommendations
          .getOrElse(List.empty)
          .map(_.medication.getOrElse(List.empty).toSet)
          .map(_.map(_.code))
          .fold(Set.empty[Medication.Code])(_ ++ _)

      val usedDrugCodes =
        mtbfile.molecularTherapies
          .getOrElse(List.empty)
          .filter(_.history.headOption isDefined)
          .map(_.history.head)
          .map {
            case th: OngoingTherapy   => th.medication.getOrElse(List.empty).toSet
            case th: StoppedTherapy   => th.medication.getOrElse(List.empty).toSet 
            case th: CompletedTherapy => th.medication.getOrElse(List.empty).toSet 
            case _                    => Set.empty[Medication.Coding]
          }
          .map(_.map(_.code))
          .fold(Set.empty[Medication.Code])(_ ++ _)


      val mutatedGeneIds =
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

          geneIds <- snvGeneIds ++ cnvGeneIds //TODO: genes from RNA-/DNA-Fusion and RNA-Seq

       } yield geneIds


      matchesQuery(mutatedGeneIds, mutatedGeneIdSelection) &&
      matchesQuery(mtbfile.diagnoses.getOrElse(List.empty).map(_.icd10.get.code), diagnosesSelection) &&
      matchesQuery(mtbfile.histologyReports.getOrElse(List.empty).flatMap(_.tumorMorphology).map(_.value.code), morphologySelection) &&
      matchesQuery(mtbfile.responses.getOrElse(List.empty).map(_.value.code), responsesSelection) &&
      matchesQuery(usedDrugCodes, usedDrugSel.map(_.medication.code)) &&
      matchesQuery(recommendedDrugCodes, recDrugSel.map(_.medication.code))

  }

}


class FSBackedLocalDB private (
  dataDir: File,
  cache: Map[Patient.Id,Snapshot[MTBFile]] 
)(
  implicit jsf: Format[Snapshot[MTBFile]]
) extends LocalDB
{


  private def newSnapshotId: Snapshot.Id = {
    Snapshot.Id(randomUUID.toString)
  }


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
