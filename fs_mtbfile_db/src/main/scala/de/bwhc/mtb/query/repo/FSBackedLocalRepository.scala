package de.bwhc.mtb.query.repo


import java.io.{
  File,
  FileWriter,
  InputStream,
  FileInputStream
}
import java.time.Instant
import java.util.UUID.randomUUID

import scala.util.{Failure,Success}
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


  lazy val instance: FSBackedLocalDB = {

    if (!dataDir.exists) dataDir.mkdirs

    val initData =
      Option {
        dataDir.listFiles((_,n) => n.startsWith("Patient_") && n.endsWith(".json"))
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
//      .getOrElse(LazyList.empty[Snapshot[MTBFile]])
      .groupBy(_.data.patient.id)
      .view
      .mapValues(_.sortWith((s1,s2) => s1.timestamp.isAfter(s2.timestamp)))
      .mapValues(_.toList)
      .toSeq

    new FSBackedLocalDB(
      dataDir,
      TrieMap(initData: _*) 
    )

  }


  import Query._
  import DrugUsage._
  import Variant.Gene

  import scala.language.implicitConversions

  implicit def toPredicate(params: Parameters): Snapshot[MTBFile] => Boolean = {

    case Snapshot(_,_,mtbfile) =>

      def matchesSelection[T](
        vals: Iterable[T],
        selection: Set[T]
      ): Boolean = {
        selection.isEmpty || vals.exists(v => selection contains v)
      }


      val (usedDrugSel,recDrugSel) = params.medicationsWithUsage.partition(_.usage == Used)

      val recommendedDrugCodes =
        mtbfile.recommendations
          .getOrElse(List.empty[TherapyRecommendation])
          .map(_.medication.toList.toSet)
          .map(_.map(_.code))
          .fold(Set.empty[Medication])(_ ++ _)

      val usedDrugCodes =
        mtbfile.molecularTherapies
          .getOrElse(List.empty[MolecularTherapyDocumentation])
          .filter(_.history.headOption isDefined)
          .map(_.history.head)
          .map {
            case th: OngoingTherapy   => th.medication.toList.toSet
            case th: StoppedTherapy   => th.medication.toList.toSet 
            case th: CompletedTherapy => th.medication.toList.toSet 
            case _                    => Set.empty[Coding[Medication]]
          }
          .map(_.map(_.code))
          .fold(Set.empty[Medication])(_ ++ _)


      val matchingMutatedGene = 
        (for {
          ngs <- mtbfile.ngsReports.getOrElse(List.empty[SomaticNGSReport])

          snvGenes = ngs.simpleVariants.getOrElse(List.empty[SimpleVariant])
                       .map(_.gene.code)

          cnvGenes = ngs.copyNumberVariants.getOrElse(List.empty[CNV])
                       .flatMap(_.reportedAffectedGenes.getOrElse(List.empty[Coding[Gene]]).map(_.code))

        } yield (matchesSelection(snvGenes,params.mutatedGenes) || matchesSelection(cnvGenes,params.mutatedGenes))
        ).exists(_ == true)


      matchingMutatedGene &&
      matchesSelection(mtbfile.diagnoses.getOrElse(List.empty[Diagnosis]).map(_.icd10.get.code), params.diagnoses) &&
      matchesSelection(mtbfile.responses.getOrElse(List.empty[Response]).map(_.value.code), params.responses) &&
      matchesSelection(usedDrugCodes, usedDrugSel.map(_.code)) &&
      matchesSelection(recommendedDrugCodes, recDrugSel.map(_.code))

  }

}


class FSBackedLocalDB private (
  dataDir: File,
  cache: Map[Patient.Id,List[Snapshot[MTBFile]]] 
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

    val patId   = mtbFile.patient.id
    val history = cache.getOrElse(patId, List.empty[Snapshot[MTBFile]])

    val saved =
      for {
        snp <- Future.successful(Snapshot(newSnapshotId,Instant.now,mtbFile))
        _   =  cache.update(patId, snp :: history)
        _   <- Future {
                 val writer =
                   new FileWriter(
                     new File(
                       dataDir,
                       s"Patient_${patId.value}_Snapshot_${snp.id.value}.json"
                     )
                   )
                 writer.write(Json.prettyPrint(Json.toJson(snp)))
                 writer.close
               }
      } yield snp

    saved andThen {
      case Failure(_) => cache.update(patId,history)
    }

  }


  def latestSnapshot(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[Snapshot[MTBFile]]] = {
    Future.successful(
      cache.get(patId).flatMap(_.headOption)
    )
  }


  def history(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[History[MTBFile]]] = {
    Future.successful(
      cache.get(patId).map(History(_))
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
      cache.values.map(_.headOption).filter(_.isDefined).map(_.get)
    )
  }


  def delete(
    patId: Patient.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[History[MTBFile]] = {
    Future {
      dataDir.listFiles((_,n) => n startsWith s"Patient_${patId.value}")
        .to(LazyList)
        .foreach(_.delete)
    }
    .map { _ =>
      cache.remove(patId)
        .map(History(_))
        .getOrElse(History(List.empty[Snapshot[MTBFile]]))
    }
  }


}
