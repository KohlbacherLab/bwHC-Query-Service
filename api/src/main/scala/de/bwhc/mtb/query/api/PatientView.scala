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
  id: Patient.Id,
  managingZPM: Option[ZPM],
  gender: Gender.Value,
  age: Option[Int], //TODO Quantity with Unit of time
  vitalStatus: VitalStatus.Value
)

object PatientView
{

  // INFO: this duplication of ops for Patient.age and vitalStatus,
  // already defined as extension methods, is required because of weird 
  // stackoverflow bugs when extension ops are imported
  // TODO: look for solution

  import java.time.temporal.ChronoUnit.YEARS

  implicit val fromPatient: Patient => PatientView = {
    pat =>
      PatientView(
        pat.id,
        pat.managingZPM,
        pat.gender,
        pat.birthDate.map(
          bd => YEARS.between(bd,pat.dateOfDeath.getOrElse(LocalDate.now)).toInt
        ),
        pat.dateOfDeath.map(_ => VitalStatus.Deceased).getOrElse(VitalStatus.Alive)
      )
  }

  implicit val format = Json.format[PatientView]

}
