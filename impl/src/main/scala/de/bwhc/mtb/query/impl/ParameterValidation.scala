package de.bwhc.mtb.query.impl


import cats.data.Ior
import cats.data.Validated.{validNel,invalidNel}
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
import de.bwhc.mtb.query.api.Query._


/*
// Regular expressions for Varnomen (Variant Nomenclature) validation
// See: https://varnomen.hgvs.org/
object Varnomen
{

  import scala.util.matching.Regex

  object DNA
  {

    private val nucleotideSymbols =
     Seq("A","C","G","T","B","D","H","K","M","N","R","S","V","W","Y")

    private val prefix            = "(c\\.|g\\.|m\\.|n\\.)"
    private val offset            = "([+-]\\d+)"
    private val position          = s"(\\d+$offset?)"
    private val positionRange     = s"(\\d+_\\d+$offset?)"
    private val nucleotide        = s"[${nucleotideSymbols.mkString}]"

    private val substitution      = s"($position|$positionRange)$nucleotide+>$nucleotide+"
    private val deletion          = s"($position|$positionRange)del"
    private val duplication       = s"($position|$positionRange)dup"
    private val insertion         = s"${positionRange}ins$nucleotide+"
    private val invertion         = s"${positionRange}inv".r
    private val deletionInsertion = s"($position|$positionRange)delins$nucleotide+"
    private val change =
      Seq(
        substitution,
        deletion,
        duplication,
        insertion,
        invertion,
        deletionInsertion
      )
      .mkString("|")

    private val oneAllele  = s"\\[($change);($change)\\]"
    private val twoAlleles = s"\\[($change)\\];\\[($change)\\]"


    val Substitution      = s"$prefix$substitution".r
    val Deletion          = s"$prefix$deletion".r
    val Duplication       = s"$prefix$duplication".r
    val Insertion         = s"$prefix$insertion".r
    val Invertion         = s"$prefix$invertion".r
    val DeletionInsertion = s"$prefix$deletionInsertion".r
    val Change            = s"$prefix($change)".r
    val OneAllele         = s"$prefix$oneAllele".r
    val TwoAlleles        = s"$prefix$twoAlleles".r
    val Allele            = s"$prefix($oneAllele|$twoAlleles)".r


  }


  object Protein
  {

    private val aminoAcidCodes =
      Set(
        "Ala","Asx","Cys","Asp","Glu",
        "Phe","Gly","His","Ile","Lys",
        "Leu","Met","Asn","Pro","Gln",
        "Arg","Ser","Thr","Sec","Val",
        "Trp","Tyr","Xaa","Glx",
        "*"
      )

    private val aaCode = s"(${aminoAcidCodes.mkString("|")})"

  }

}
*/

object ParameterValidation extends Validator[String,Parameters]
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


  implicit val medicationCodeValidator: Validator[String,MedicationWithUsage] = {

    case mwu @ MedicationWithUsage(medication,usageSet) =>

      atc.findWithCode(medication.code.value) mustBe defined otherwise (
        s"Invalid ATC Medication code ${medication.code.value}"
      ) map (
       _.get
      ) map (
        med =>
          mwu.copy(
            medication =
              medication.copy(
                display = Some(med.name),
                version = Some(med.version.toString)
              ),
            usage =
              usageSet.map(
                u => u.copy(display = ValueSet[DrugUsage.Value].displayOf(u.code))
              )
          )
      )
  }

  implicit val snvParametersValidator: Validator[String,SNVParameters] = {
    params =>
      validate(params.gene).map(coding => params.copy(gene = coding))
  }


  implicit val cnvParametersValidator: Validator[String,CNVParameters] = {
    case params @ CNVParameters(genes,typ,copyNumber) =>

    (
      if (genes.nonEmpty) validateEach(genes)
      else invalidNel("CNV-Parameters: Genes must be non-empty")
    )
    .map(
      codings => params.copy(genes = codings)
    )
  }


  implicit val fusionParametersValidator: Validator[String,FusionParameters] = {
    case params @ FusionParameters(fivePr,threePr) =>

      Ior.fromOptions(fivePr,threePr) mustBe (defined) otherwise (
        "Fusion Parameters: at least one of 5' and 3' fusion partner gene MUST be selected"
      ) andThen {
        _.get.fold(
          fvPr  =>
            validate(fvPr)
              .map(coding => params.copy(fivePrimeGene = Some(coding))),

          thrPr =>
            validate(thrPr)
              .map(coding => params.copy(threePrimeGene = Some(coding))),

          (fvPr,thrPr) => 
            (
             validate(fvPr),
             validate(thrPr)
            )
            .mapN(
              (fv,th) => params.copy(fivePrimeGene = Some(fv),threePrimeGene = Some(th)))
            )
      }
  }

/*
  override def apply(params: Parameters): ValidatedNel[String,Parameters] = {

    import ValueSets._

    // Validate that at least 1 parameter is set (diagnosis, or variant or medication)

    val diagnoses            = params.diagnoses.getOrElse(List.empty)
    val tumorMorphology      = params.tumorMorphology.getOrElse(List.empty)
    val mutatedGenes         = params.mutatedGenes.getOrElse(List.empty)
    val simpleVariants       = params.simpleVariants.getOrElse(List.empty)    
    val copyNumberVariants   = params.copyNumberVariants.getOrElse(List.empty)
    val dnaFusions           = params.dnaFusions.getOrElse(List.empty)
    val rnaFusions           = params.rnaFusions.getOrElse(List.empty)
    val medicationsWithUsage = params.medicationsWithUsage.getOrElse(List.empty)

    diagnoses mustBe nonEmpty orElse (
      tumorMorphology mustBe nonEmpty
    ) orElse (
      mutatedGenes mustBe nonEmpty
    ) orElse (
      simpleVariants mustBe nonEmpty
    ) orElse (
      copyNumberVariants mustBe nonEmpty
    ) orElse (
      dnaFusions mustBe nonEmpty
    ) orElse (
      rnaFusions mustBe nonEmpty
    ) orElse (
      medicationsWithUsage mustBe nonEmpty
    ) otherwise (
      "At least one of the following query parameters must be set: diagnosis, tumor morphology, genes with any alteration, SNVs, CNVs, DNA- or RNA-Fusion, or medication"
    ) andThen (_ =>
      (
        validateEach(diagnoses),     
        validateEach(tumorMorphology),
        validateEach(mutatedGenes),
        validateEach(simpleVariants),
        validateEach(copyNumberVariants),
        validateEach(dnaFusions),
        validateEach(rnaFusions),
        validateEach(medicationsWithUsage)
      )
      .mapN {
        (diags,tumorMorph,genes,snvs,cnvs,dnaFns,rnaFns,medsWithUsage) =>
          Parameters(
            Some(diags),
            Some(tumorMorph),
            Some(genes),
            Some(snvs),
            Some(cnvs),
            Some(dnaFns),
            Some(rnaFns),
            Some(medsWithUsage),
            params.responses.map(
              _.map(
                c => c.copy(display = ValueSet[RECIST.Value].displayOf(c.code))
              )
            )
          )
      }
    )

  } 
*/


  override def apply(params: Parameters): ValidatedNel[String,Parameters] = {

    import ValueSets._

    // TODO: validate that at least 1 parameter is set (diagnosis, or variant or medication)

    (
      validateEach(params.diagnoses.getOrElse(List.empty)),
      validateEach(params.tumorMorphology.getOrElse(List.empty)),
      validateEach(params.mutatedGenes.getOrElse(List.empty)),
      validateEach(params.simpleVariants.getOrElse(List.empty)),
      validateEach(params.copyNumberVariants.getOrElse(List.empty)),
      validateEach(params.dnaFusions.getOrElse(List.empty)),
      validateEach(params.rnaFusions.getOrElse(List.empty)),
      validateEach(params.medicationsWithUsage.getOrElse(List.empty))
    )
    .mapN {
      (diags,tumorMorphology,genes,snvs,cnvs,dnaFusions,rnaFusions,medsWithUsage) =>
        Parameters(
          Some(diags),
          Some(tumorMorphology),
          Some(genes),
          Some(snvs),
          Some(cnvs),
          Some(dnaFusions),
          Some(rnaFusions),
          Some(medsWithUsage),
          params.responses.map(
            _.map(
              c => c.copy(display = ValueSet[RECIST.Value].displayOf(c.code))
            )
          )
        )
    }

  } 


}
