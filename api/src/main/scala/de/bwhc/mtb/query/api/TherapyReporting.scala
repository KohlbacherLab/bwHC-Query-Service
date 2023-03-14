package de.bwhc.mtb.query.api


import java.time.LocalDateTime
import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}
import cats.data.{NonEmptyList,IorNel}
import play.api.libs.json._
import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICD10GM,
  Medication,
  Patient,
  MolecularTherapy,
  Response,
  ZPM
}
import de.bwhc.mtb.data.entry.views.SupportingVariantDisplay



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
    medication: Option[Medication.Coding]
  )

  object Filters
  {

    val empty = Filters(None)

    implicit val format =
      Json.format[Filters]
  }

/*
  implicit def format[T] =
    Format[Report[T]](
      Reads(
        js =>
          js.validate[LocalReport[T]]
            .orElse(js.validate[GlobalReport[T]])
      ),
      Writes { 
        case r: LocalReport[T] => Json.toJson(r)
        case r: LocalReport[T] => Json.toJson(r)
      }
    )
*/
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



final case class TherapyResponse
(
  therapy: MolecularTherapy,
  supportingVariants: Option[List[SupportingVariantDisplay]],
  response: Option[Response]
)

final case class PatientTherapies
(
  patient: Patient,
  therapies: Seq[TherapyResponse]
)


object PatientTherapies
{

  implicit val formatTherapyResponse = Json.format[TherapyResponse]
  implicit val formatPatientTherapies = Json.format[PatientTherapies]
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
