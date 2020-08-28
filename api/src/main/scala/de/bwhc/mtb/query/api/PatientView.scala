package de.bwhc.mtb.query.api



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Gender,
  Patient,
  ZPM
}


object VitalStatus extends Enumeration
{
  val Alive, Deceased = Value

  implicit val format = Json.formatEnum(this)
}



case class PatientView
(
  pseudonym: Patient.Id,
  managingZPM: Option[ZPM],
  gender: Gender.Value,
  age: Option[Int], //TODO Quantity with Unit of time
  vitalStatus: VitalStatus.Value
)

object PatientView
{

  import extensions._

  implicit val fromPatient: Patient => PatientView = {
    pat =>
      PatientView(
        pat.id,
        pat.managingZPM,
        pat.gender,
        pat.age,
        pat.vitalStatus
      )
  }

  implicit val format = Json.format[PatientView]
}
