package de.bwhc.mtb.query.api


import java.time.{
    LocalDate,
    LocalDateTime
}
import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}
import cats.data.{NonEmptyList,IorNel}
import play.api.libs.json._
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  Medication,
  Patient,
  MolecularTherapy,
  Response,
  ZPM
}
import de.bwhc.mtb.views.SupportingVariantDisplay



final case class ConceptCount[T]
(
  concept: T,
  count: Int,
  components: Option[Seq[ConceptCount[T]]]
)

object ConceptCount
{
  import play.api.libs.functional.syntax._

  // Explicit Format definition required because of recursive type ConceptCount
  implicit def format[T: Format]: Format[ConceptCount[T]] =
    (
      (JsPath \ "concept").format[T] and
      (JsPath \ "count").format[Int] and
      (JsPath \ "components").lazyFormatNullable(Format.of[Seq[ConceptCount[T]]]) // lazyFormat to handle recursivity
    )(
      ConceptCount.apply _,
      unlift(ConceptCount.unapply)
    )

  // Order ConceptCounts by decreasing number of occurrence
  implicit def ordering[T] =
    Ordering[Int]
      .on[ConceptCount[T]](_.count)
      .reverse

}


sealed trait Report[T]
{
  val compiledOn: LocalDateTime
  val scope: Report.Scope.Value
  val filters: Report.Filters
  val data: T 
}

object Report
{

  object Scope extends Enumeration
  {
    val Local  = Value("local")
    val Global = Value("global")

    implicit val format = Json.formatEnum(this)
  }

  final case class Filters
  (
    medication: Option[Medication.Coding],
    medicationUsage: Option[Query.DrugUsage.Value]
  )

  object Filters
  {

    val empty =
      Filters(None,None)

    implicit val format =
      Json.format[Filters]
  }

}


final case class LocalReport[T]
(
  compiledOn: LocalDateTime,
  site: ZPM,
  filters: Report.Filters,
  data: T
)
extends Report[T]
{
  val scope = Report.Scope.Local
}

final case class GlobalReport[T]
(
  compiledOn: LocalDateTime,
  sites: List[ZPM],
  filters: Report.Filters,
  data: T,
  components: Option[Seq[LocalReport[T]]]
)
extends Report[T]
{
  val scope = Report.Scope.Global
}

object LocalReport
{
  implicit def writes[T: Writes] =
    Json.writes[LocalReport[T]]
      .transform(
        (js: JsValue) => 
          js.as[JsObject] + ("scope" -> Json.toJson(Report.Scope.Local))
      )

  implicit def reads[T: Reads] =
    Json.reads[LocalReport[T]]
}

object GlobalReport
{
  implicit def writes[T: Writes] =
    Json.writes[GlobalReport[T]]
      .transform(
        (js: JsValue) => 
          js.as[JsObject] + ("scope" -> Json.toJson(Report.Scope.Global))
      )

  implicit def reads[T: Reads] =
    Json.reads[GlobalReport[T]]
}



final case class TherapyWithResponse
(
  therapy: MolecularTherapy,
  supportingVariants: Option[List[SupportingVariantDisplay]],
  response: Option[Response]
)

final case class PatientTherapies
(
  patient: Patient,
  therapies: Seq[TherapyWithResponse]
)


object PatientTherapies
{

  implicit val formatTherapyWithResponse = Json.format[TherapyWithResponse]
  implicit val formatPatientTherapies = Json.format[PatientTherapies]


  object csv
  {

    import de.bwhc.util.csv.CsvWriter
    import CsvWriter.derivation._
    import CsvWriter.temporal._
    import de.bwhc.mtb.dtos.{
      Period,
      OpenEndPeriod,
      ClosedPeriod,
      ValueSet,
      ValueSets
    }
    import ValueSets._
    import extensions._


    type PatientTherapyTuple =
      (Patient,MolecularTherapy,Option[List[SupportingVariantDisplay]],Option[Response])


    def denormalize(
      pth: PatientTherapies
    ): Seq[PatientTherapyTuple] =
      pth.therapies.map {
        th => (pth.patient,th.therapy,th.supportingVariants,th.response)
      }

    implicit def enumDisplayWriter[E <: Enumeration](
      implicit vs: ValueSet[E#Value]
    ): CsvWriter[E#Value] =
      CsvWriter[E#Value](e => vs.displayOf(e).get)

    implicit val medicationListWriter =
      CsvWriter[List[Medication.Coding]](
        _.flatMap(_.display)
         .mkString(",")
      )


    implicit val writer =
      CsvWriter.of[PatientTherapyTuple](
        "Patient-ID"                -> CsvWriter.on[PatientTherapyTuple]{ case (pat,_,_,_) => pat.id},
        "Standort"                  -> CsvWriter.on[PatientTherapyTuple]{ case (pat,_,_,_) => pat.managingZPM},
        "Alter"                     -> CsvWriter.on[PatientTherapyTuple]{ case (pat,_,_,_) => pat.age },
        "Geschlecht"                -> CsvWriter.on[PatientTherapyTuple]{ case (pat,_,_,_) => pat.gender },
        "Vital-Status"              -> CsvWriter.on[PatientTherapyTuple]{ case (pat,_,_,_) => pat.vitalStatus },
        "ICD-10"                    -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.reason.map(_.code) },
        "Entität"                   -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.reason.map(_.display) },
        "Therapie-Status"           -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.status },
        "Nicht-Umsetzungs-Grund"    -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.notDoneReason.map(_.code) },
        "Abbruchsgrund"             -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.reasonStopped.map(_.code) },
        "Stützende Mol. Alteration" -> CsvWriter.on[PatientTherapyTuple]{ case (_,_,supp,_) => supp.map(_.map(_.value).mkString(",")) },
        "Anfangs-Datum"             -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) =>  th.period.map(_.start) },
        "End-Datum"                 -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => 
                                         th.period.flatMap {
                                           case p: OpenEndPeriod[LocalDate] => p.end
                                           case p: ClosedPeriod[LocalDate]  => Some(p.end)
                                         }
                                       },
        "Medikation"                -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.medication },
        "Datum des Follow-ups"      -> CsvWriter.on[PatientTherapyTuple]{ case (_,th,_,_) => th.recordedOn },
        "Response"                  -> CsvWriter.on[PatientTherapyTuple]{ case (_,_,_,resp) => resp.map(_.value.code) },
      )

  }

}


object ReportingAliases
{

  type Distribution[T] =
    Seq[ConceptCount[T]]

  type LocalDistributionReport[T] =
    LocalReport[Distribution[T]]

  type GlobalDistributionReport[T] =
    GlobalReport[Distribution[T]]

  type LocalMedicationDistributionReport =
    LocalDistributionReport[Medication.Coding]

  type GlobalMedicationDistributionReport =
    GlobalDistributionReport[Medication.Coding]

  type LocalTumorEntityDistributionReport =
    LocalDistributionReport[Coding[ICD10GM]]

  type GlobalTumorEntityDistributionReport =
    GlobalDistributionReport[Coding[ICD10GM]]

}
 


trait TherapyReportingOps
{

  import ReportingAliases._

  def compileLocalMedicationDistributionFor(
    request: PeerToPeerRequest[Report.Filters]
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],LocalMedicationDistributionReport]]

  
  def compileGlobalMedicationDistribution(
    medicationUsage: Option[Query.DrugUsage.Value]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalMedicationDistributionReport]]



  def compileLocalTumorEntityDistributionFor(
    request: PeerToPeerRequest[Report.Filters]
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],LocalTumorEntityDistributionReport]]

  
  def compileGlobalTumorEntityDistribution(
    medication: Option[Medication.Coding]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalTumorEntityDistributionReport]]



  def compilePatientTherapies(
    request: PeerToPeerRequest[Report.Filters]
  )(
    implicit
    ec: ExecutionContext
  ): Future[Either[NonEmptyList[String],LocalReport[Seq[PatientTherapies]]]]

  
  def compileGlobalPatientTherapies(
    medication: Option[Medication.Coding]
  )(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalReport[Seq[PatientTherapies]]]]


}
