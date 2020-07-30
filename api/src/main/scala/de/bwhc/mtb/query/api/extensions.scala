package de.bwhc.mtb.query.api



import java.time.LocalDate
import java.time.temporal.ChronoUnit.YEARS

import de.bwhc.mtb.data.entry.dtos.Patient


object extensions
{

  implicit class PatientOps(val pat: Patient) extends AnyVal
  {

    def age: Option[Int] =
      pat.birthDate
        .map(
          YEARS.between(_,pat.dateOfDeath.getOrElse(LocalDate.now)).toInt
        )

    def vitalStatus: VitalStatus.Value =
      pat.dateOfDeath
        .map(_ => VitalStatus.Deceased)
        .getOrElse(VitalStatus.Alive)

  }


}
