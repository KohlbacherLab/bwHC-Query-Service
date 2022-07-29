package de.bwhc.mtb.query.impl



import de.bwhc.catalogs.icd.{ICD10GMCatalogs,ICDO3Catalogs}

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICDO3M,
  ICD10GM,
  Medication
}
import de.bwhc.catalogs.med.{MedicationCatalog}
import de.bwhc.catalogs.med.{Medication => ATC}
import de.bwhc.mtb.query.api.Query.{Parameters,MedicationWithUsage}


/*
  Processor of Query.Parameters to automatically:
  - include all sub-categories of ICD-10 super-categories
  - include all sub-categories of ICD-O-3-M super-categories
  - include all substances of ATC groups 
  into the query
*/
object ParameterProcessor extends (Parameters => Parameters)
{

  val icd10s =
    ICD10GMCatalogs.getInstance.get.codings()

  val icdO3ms =
    ICDO3Catalogs.getInstance.get.morphologyCodings()

  val atc =
    MedicationCatalog.getInstance.get


  def apply(params: Parameters): Parameters = {
  
    val expandedDiagnoses =
      params.diagnoses
        .getOrElse(List.empty)
        .flatMap {
          coding => 
          
          icd10s.find(_.code.value == coding.code.value)
            .map(
              _.subClasses
               .map(c => Coding(ICD10GM(c.value),None))
               .toList
            )
            .getOrElse(List.empty) :+ coding 

        }

    val expandedMorphologies =
      params.tumorMorphology
        .getOrElse(List.empty)
        .flatMap {
          coding => 
          
          icdO3ms.find(_.code.value == coding.code.value)
            .map(
              _.subClasses
               .map(c => Coding(ICDO3M(c.value),None))
               .toList
            )
            .getOrElse(List.empty) :+ coding

        }

    val expandedDrugs =
      params.medicationsWithUsage
        .getOrElse(List.empty)
        .flatMap {
          case coding @ MedicationWithUsage(medication,usage) =>
         
            atc.find(ATC.Code(medication.code.value),atc.latestVersion)
              .map(
                _.children.map( c =>
                  MedicationWithUsage(
                    Coding(Medication.Code(c.value),None),
                    usage
                  )
                )
                .toList
              )
              .getOrElse(List.empty) :+ coding
        }

    params.copy(
      diagnoses = Some(expandedDiagnoses),
      tumorMorphology = Some(expandedMorphologies),
      medicationsWithUsage = Some(expandedDrugs)
    )

  }

}


