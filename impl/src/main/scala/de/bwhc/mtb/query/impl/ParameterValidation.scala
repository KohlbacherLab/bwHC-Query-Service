package de.bwhc.mtb.query.impl



import cats.data.Validated.validNel
import cats.data.ValidatedNel
import de.bwhc.util.data.Validation._
import de.bwhc.util.data.Validation.dsl._
import de.bwhc.catalogs.icd.{ICD10GMCatalogs,ICDO3Catalogs}
import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalog,HGNCId}
import de.bwhc.catalogs.med.MedicationCatalog

import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  ICDO3M,
  ICD10GM,
  Medication,
  Gene,
  RECIST,
  ValueSet,
  ValueSets,
}
import de.bwhc.mtb.query.api.Query


object ParameterValidation extends Validator[String,Query.Parameters]
{

  import cats.syntax.apply._
  import cats.instances.list._  

  import scala.language.implicitConversions


  implicit val icd10s =
    ICD10GMCatalogs.getInstance.get.codings()

  implicit val icdO3ms =
    ICDO3Catalogs.getInstance.get.morphologyCodings()

  implicit val hgnc =
    HGNCCatalog.getInstance.get

  implicit val atc =
    MedicationCatalog.getInstance.get


  implicit val icd10codingValidator: Validator[String,Coding[ICD10GM]] = {
    coding =>
      icd10s.find(_.code.value == coding.code.value) mustBe defined otherwise (
        s"Invalid ICD-10-GM code ${coding.code.value}"
      ) map (
        _.get
      ) map (
        icd10 =>
          coding.copy(
            display = Some(icd10.display),
            version = Some(icd10.version.toString)
          )
      )
  }


  implicit val icdO3McodingValidator: Validator[String,Coding[ICDO3M]] = {
    coding =>
      icdO3ms.find(_.code.value == coding.code.value) mustBe defined otherwise (
        s"Invalid ICD-O-3-M code ${coding.code.value}"
      ) map (
        _.get
      ) map (
        icdo3m =>
          coding.copy(
            display = Some(icdo3m.display),
            version = Some(icdo3m.version.toString)
          )
      )
  }


  implicit val hgncIdValidator: Validator[String,Coding[Gene.HgncId]] = {
    coding =>
      hgnc.gene(HGNCId(coding.code.value)) mustBe defined otherwise (
        s"Invalid HGNC-ID ${coding.code.value}"
      ) map (
       _.get
      ) map (
        gene => coding.copy(display = Some(gene.name)) 
      )

  }


  implicit val medicationCodeValidator: Validator[String,Query.MedicationWithUsage] = {
    mwu =>
      atc.entries().find(_.code.value == mwu.medication.code.value) mustBe defined otherwise (
        s"Invalid ATC Medication code ${mwu.medication.code.value}"
      ) map (
       _.get
      ) map (
        med =>
          mwu.copy(
            medication =
              mwu.medication.copy(
                display = Some(med.name),
                version = Some(med.version.toString)
              )
          )
      )
  }


  override def apply(params: Query.Parameters): ValidatedNel[String,Query.Parameters] = {

    import ValueSets._

    (
      validateEach(params.diagnoses.getOrElse(Set.empty).toList),

      validateEach(params.tumorMorphology.getOrElse(Set.empty).toList),

      validateEach(params.mutatedGenes.getOrElse(Set.empty).toList),

      validateEach(params.medicationsWithUsage.getOrElse(Set.empty).toList)
    )
    .mapN {
      (diags,tumorMorphology,genes,mwus) =>
        Query.Parameters(
          Some(diags.toSet),
          Some(tumorMorphology.toSet),
          Some(genes.toSet),
          Some(mwus.toSet),
          params.responses.map(
            _.map(
              c => c.copy(display = ValueSet[RECIST.Value].displayOf(c.code))
            )
          )
        )
    }
  } 

}


