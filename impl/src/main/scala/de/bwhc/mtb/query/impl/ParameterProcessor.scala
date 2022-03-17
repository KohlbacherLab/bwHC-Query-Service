package de.bwhc.mtb.query.impl



import de.bwhc.catalogs.icd.{ICD10GMCatalogs,ICDO3Catalogs}

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICDO3M,
  ICD10GM,
}
import de.bwhc.mtb.query.api.Query


object ParameterProcessor extends (Query.Parameters => Query.Parameters)
{

  val icd10s =
    ICD10GMCatalogs.getInstance.get.codings()

  val icdO3ms =
    ICDO3Catalogs.getInstance.get.morphologyCodings()


  def apply(params: Query.Parameters): Query.Parameters = {
  
    val expandedDiagnoses =
      params.diagnoses
        .getOrElse(Set.empty)
        .flatMap {
          coding => 
          
          icd10s.find(_.code.value == coding.code.value)
            .map(_.subClasses.map(c => Coding(ICD10GM(c.value),None)))
            .getOrElse(Set.empty) + coding 

        }

    val expandedMorphologies =
      params.tumorMorphology
        .getOrElse(Set.empty)
        .flatMap {
          coding => 
          
          icdO3ms.find(_.code.value == coding.code.value)
            .map(_.subClasses.map(c => Coding(ICDO3M(c.value),None)))
            .getOrElse(Set.empty) + coding

        }

    params.copy(
      diagnoses = Some(expandedDiagnoses),
      tumorMorphology = Some(expandedMorphologies)
    )

  }

}


