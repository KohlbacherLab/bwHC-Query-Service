package de.bwhc.mtb.query.api



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Patient,
  ZPM,
  ValueSet => ValSet
}
import de.bwhc.mtb.data.entry.views.{
  Or,
  NotAvailable,
}


object VitalStatus extends Enumeration
{
  val Alive, Deceased = Value

  implicit val format = Json.formatEnum(this)
  implicit val system = Coding.System[VitalStatus.Value]("Vital-Status")


  implicit val vitalStatusDE =
    ValSet[VitalStatus.Value](
      "Vital-Status",
      VitalStatus.Alive    -> "Lebend",
      VitalStatus.Deceased -> "Verstorben"
    )

}

case class DiagnosisSummary(value: String) extends AnyVal
object DiagnosisSummary
{
  implicit val format = Json.valueFormat[DiagnosisSummary]
}


final case class PatientView
(
  id: Patient.Id,
  managingZPM: NotAvailable Or ZPM,
  gender: String,
  diagnosis: NotAvailable Or DiagnosisSummary,
  age: NotAvailable Or Int,
  vitalStatus: String
)


object PatientView
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[PatientView]
}
