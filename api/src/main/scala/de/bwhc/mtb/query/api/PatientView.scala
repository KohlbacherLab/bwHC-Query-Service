package de.bwhc.mtb.query.api



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Gender,
  Patient,
  ZPM,
  ValueSet
}
import de.bwhc.mtb.data.entry.views.{
  Or,
  NotAvailable
}


object VitalStatus extends Enumeration
{
  val Alive, Deceased = Value

  implicit val format = Json.formatEnum(this)

}


final case class PatientView
(
  id: Patient.Id,
  managingZPM: NotAvailable Or ZPM,
  gender: String,
  age: NotAvailable Or Int,
  vitalStatus: String
)


object PatientView
{

  // INFO: this duplication of ops for Patient.age and vitalStatus,
  // already defined as extension methods, is required because of weird 
  // stackoverflow bugs when extension ops are imported
  // TODO: look for solution

  import de.bwhc.mtb.data.entry.dtos.ValueSets._

  import java.time.temporal.ChronoUnit.YEARS

  implicit val vitalStatusDE: ValueSet[VitalStatus.Value] =
    ValueSet(
      "Vital-Status",
      VitalStatus.Alive    -> "Lebend",
      VitalStatus.Deceased -> "Verstorben"
    )


  implicit val fromPatient: Patient => PatientView = {
    pat =>
      PatientView(
        pat.id,
        pat.managingZPM.toRight(NotAvailable),
        ValueSet[Gender.Value].displayOf(pat.gender).get,
        pat.birthDate.map(
          bd => YEARS.between(bd,pat.dateOfDeath.getOrElse(LocalDate.now)).toInt
        ).toRight(NotAvailable),
        ValueSet[VitalStatus.Value].displayOf(
          pat.dateOfDeath.map(_ => VitalStatus.Deceased).getOrElse(VitalStatus.Alive)
        ).get
      )
  }


  import de.bwhc.util.json._

  implicit val format = Json.writes[PatientView]

}
