package de.bwhc.mtb.query.impl



import cats.data.Validated.validNel
import cats.data.ValidatedNel
import de.bwhc.util.data.Validation._
import de.bwhc.util.data.Validation.dsl._
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalog,HGNCId}
import de.bwhc.catalogs.med.MedicationCatalog

import de.bwhc.mtb.data.entry.dtos.{
  ICD10GM,
  Medication,
  Gene
}
import de.bwhc.mtb.query.api.Query



object ParameterValidation extends Validator[String,Query.Parameters]
{

  import cats.syntax.apply._
  import cats.instances.set._  
  import cats.instances.list._  

  import scala.language.implicitConversions


  implicit val icd10s =
    ICD10GMCatalogs.getInstance.get.codings()

  implicit val hgnc =
    HGNCCatalog.getInstance.get

  implicit val atc =
    MedicationCatalog.getInstance.get


  implicit val icd10codeValidator: Validator[String,ICD10GM] = {
    case icd10 @ ICD10GM(code) =>
      (code must be (in (icd10s.map(_.code.value)))
        otherwise (s"Invalid ICD-10-GM code $code"))
        .map(_ => icd10)
  }


  implicit val hgncIdValidator: Validator[String,Gene.HgncId] =
    id =>
      hgnc.gene(HGNCId(id.value)) mustBe defined otherwise (
        s"Invalid HGNC-ID ${id.value}"
      ) map (_ => id)
/*
  implicit val geneSymbolValidator: Validator[String,Variant.GeneSymbol] =
    symbol =>
      (hgnc.geneWithSymbol(symbol.value).headOption mustBe defined
        otherwise(s"Invalid Gene Symbol ${symbol.value}"))
        .map(_ => symbol)
*/

  implicit val mecicationCodeValidator: Validator[String,Medication.Code] = {
    case med @ Medication.Code(code) =>
      (code must be (in (atc.entries().map(_.code.value)))
        otherwise (s"Invalid ATC Medication code $code"))
       .map(c => med)
  }


  override def apply(params: Query.Parameters): ValidatedNel[String,Query.Parameters] = {
    
    (
      params.diagnoses.fold(
        validNel[String,List[ICD10GM]](List.empty)
      )(
        _.toList.validateEach
      ),

      params.mutatedGenes.fold(
        validNel[String,List[Gene.HgncId]](List.empty)
      )(
        _.toList.validateEach
      ),

      params.medicationsWithUsage.map(_.map(_.code))
      .fold(
        validNel[String,List[Medication.Code]](List.empty)
      )(
        _.toList.validateEach
      )      
      
    )
    .mapN { case _: Product => params}
    
  }

}
