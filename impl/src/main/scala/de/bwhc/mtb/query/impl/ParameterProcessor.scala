package de.bwhc.mtb.query.impl



import de.bwhc.catalogs.icd.{
  ICD10GMCatalogs,
  ICDO3Catalogs
}
import de.bwhc.mtb.dtos.{
  Coding,
  ICDO3M,
  ICD10GM,
  Medication
}
import de.bwhc.mtb.dto.extensions.CodingExtensions._
import de.bwhc.catalogs.med.{MedicationCatalog}
import de.bwhc.catalogs.med.{Medication => ATC}
import de.bwhc.mtb.query.api.Query.{
  Parameters,
  MedicationWithUsage
}


/*
  Processor of Query.Parameters to automatically:
  - include all sub-categories of ICD-10 super-categories
  - include all sub-categories of ICD-O-3-M super-categories
  - include all substances of ATC groups 
  into the query
*/
object ParameterProcessor extends (Parameters => Parameters)
{

  implicit val icd10s =
    ICD10GMCatalogs.getInstance.get

  implicit val icdO3ms =
    ICDO3Catalogs.getInstance.get

  implicit val atc =
    MedicationCatalog.getInstance.get


  def apply(params: Parameters): Parameters = {
  
    val expandedDiagnoses =
      params.diagnoses
        .getOrElse(List.empty)
        .flatMap {
          coding => coding :: coding.subClasses.toList
        }

    val expandedMorphologies =
      params.tumorMorphology
        .getOrElse(List.empty)
        .flatMap {
          coding => coding :: coding.subClasses.toList
        }

    val expandedDrugs =
      params.medicationsWithUsage
        .getOrElse(List.empty)
        .flatMap {
          case mwu @ MedicationWithUsage(medication,usage) =>
            val coding =
              Medication.Coding(
                medication.code,
                Medication.System.ATC,
                medication.display,
                medication.version
              )
            
            (coding :: coding.childSubstances.toList).map(
              c =>
                MedicationWithUsage(
                  Coding(
                    c.code,
                    c.display,
                    c.version
                  ),
                  usage
                )
             )
        }


    params.copy(
      diagnoses = Some(expandedDiagnoses),
      tumorMorphology = Some(expandedMorphologies),
      medicationsWithUsage = Some(expandedDrugs)
    )

  }

}


