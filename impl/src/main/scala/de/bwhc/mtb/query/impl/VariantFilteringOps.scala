package de.bwhc.mtb.query.impl


import de.bwhc.mtb.dtos.{
  Coding,
  Variant,
  Gene,
  SimpleVariant,
  CNV,
  DNAFusion,
  RNAFusion,
}
import de.bwhc.mtb.views.{
  SimpleVariantView,
  CNVView,
  DNAFusionView,
  RNAFusionView
}
import de.bwhc.mtb.query.api.Query.{
  SNVParameters,
  CNVParameters,
  FusionParameters,
}


trait VariantFilteringOps
{

  import scala.language.implicitConversions

  implicit def snvParametersToPredicate(params: SNVParameters): SimpleVariant => Boolean = {

    snv =>

      import SimpleVariant.{DNAChange,AminoAcidChange}

      snv.gene.flatMap(_.hgncId).exists(_.value equalsIgnoreCase params.gene.code.value) &&
        params.dnaChange.fold(true)(
          pttrn => snv.dnaChange.exists(_.code.value contains pttrn.value)
        ) &&
        params.aminoAcidChange.fold(true)(
          pttrn => snv.aminoAcidChange.exists(_.code.value contains pttrn.value)
        )
  }


  implicit def cnvParametersToPredicate(params: CNVParameters): CNV => Boolean = {
    cnv =>

      val affectedGeneIds =
        cnv.reportedAffectedGenes.getOrElse(List.empty).flatMap(_.hgncId).map(_.value)

      params.genes.map(_.code.value).forall(affectedGeneIds.contains) &&
        params.`type`.fold(true)(_ == cnv.`type`) &&
          params.copyNumber.fold(true)(range => cnv.totalCopyNumber.exists(range.contains))
  }



  implicit def fusionParametersToDNAFusionPredicate(params: FusionParameters): DNAFusion => Boolean = {
    dna =>

      params.fivePrimeGene.fold(true)(
        gene => dna.fusionPartner5prime.flatMap(_.gene.hgncId).exists(_.value == gene.code.value)
      ) &&
        params.threePrimeGene.fold(true)(
          gene => dna.fusionPartner3prime.flatMap(_.gene.hgncId).exists(_.value == gene.code.value)
        )

  }



  implicit def fusionParametersToRNAFusionPredicate(params: FusionParameters): RNAFusion => Boolean = {
    rna =>

      params.fivePrimeGene.fold(true)(
        gene => rna.fusionPartner5prime.flatMap(_.gene.hgncId).exists(_.value == gene.code.value)
      ) &&
        params.threePrimeGene.fold(true)(
          gene => rna.fusionPartner3prime.flatMap(_.gene.hgncId).exists(_.value == gene.code.value)
        )

  }



  private def SnvOfInterestForParams(
    params: Option[List[SNVParameters]]
  ): SimpleVariant => Boolean = {
    snv =>
      params.filter(_.nonEmpty) match {
        case Some(snvParams) => snvParams.exists(p => p(snv))
        case None => false
      }
  }

  private def SnvOfInterestForCodings(
    params: Option[List[Coding[Gene.HgncId]]]
  ): SimpleVariant => Boolean = {
    snv =>
      params.filter(_.nonEmpty) match {
        case Some(codings) =>
          codings.exists(coding => snv.gene.flatMap(_.hgncId).exists(_.value equalsIgnoreCase coding.code.value))

        case None => false
      }
  }

  def SnvOfInterest(
    params: Option[List[SNVParameters]],
    codings: Option[List[Coding[Gene.HgncId]]]
  ): SimpleVariant => Boolean = {
    snv =>
      SnvOfInterestForParams(params)(snv) || SnvOfInterestForCodings(codings)(snv)
  }




  private def CnvOfInterestForParams(params: Option[List[CNVParameters]]): CNV => Boolean = {
    cnv =>
      params.filter(_.nonEmpty) match {
        case Some(cnvParams) => cnvParams.exists(p => p(cnv))
        case None => false
      }
  }

  private def CnvOfInterestForCodings(params: Option[List[Coding[Gene.HgncId]]]): CNV => Boolean = {
    cnv =>
      params.filter(_.nonEmpty) match {
        case Some(codings) => 
          codings.exists(coding => cnv.reportedAffectedGenes.getOrElse(List.empty).flatMap(_.hgncId).exists(_.value equalsIgnoreCase coding.code.value))
        case None => false
      }
  }

  def CnvOfInterest(
    params: Option[List[CNVParameters]],
    codings: Option[List[Coding[Gene.HgncId]]]
  ): CNV => Boolean = {
    cnv =>
      CnvOfInterestForParams(params)(cnv) || CnvOfInterestForCodings(codings)(cnv)
  }



  private def DnaFusionOfInterestForParams(
    params: Option[List[FusionParameters]]
  ): DNAFusion => Boolean = {
    fusion =>
      params.filter(_.nonEmpty) match {
        case Some(fusionParams) => fusionParams.map(fusionParametersToDNAFusionPredicate).exists(_(fusion))
        case None => false
      }
  }

  private def DnaFusionOfInterestForCodings(
    params: Option[List[Coding[Gene.HgncId]]]
  ): DNAFusion => Boolean = {
    fusion =>
      params.filter(_.nonEmpty) match {
        case Some(codings) =>
          codings.exists(coding => fusion.fusionPartner5prime.flatMap(_.gene.hgncId).exists(_.value == coding.code.value)) ||
          codings.exists(coding => fusion.fusionPartner3prime.flatMap(_.gene.hgncId).exists(_.value == coding.code.value))

        case None => false
      }
  }

  def DnaFusionOfInterest(
    params: Option[List[FusionParameters]],
    codings: Option[List[Coding[Gene.HgncId]]]
  ): DNAFusion => Boolean = {
    fusion =>
      DnaFusionOfInterestForParams(params)(fusion) || DnaFusionOfInterestForCodings(codings)(fusion)
  }




  private def RnaFusionOfInterestForParams(
    params: Option[List[FusionParameters]]
  ): RNAFusion => Boolean = {
    fusion =>
      params.filter(_.nonEmpty) match {
        case Some(fusionParams) => fusionParams.map(fusionParametersToRNAFusionPredicate).exists(_(fusion))
        case None => false
      }
  }

  private def RnaFusionOfInterestForCodings(
    params: Option[List[Coding[Gene.HgncId]]]
  ): RNAFusion => Boolean = {
    fusion =>
      params.filter(_.nonEmpty) match {
        case Some(codings) =>
          codings.exists(coding => fusion.fusionPartner5prime.flatMap(_.gene.hgncId).exists(_.value == coding.code.value)) ||
          codings.exists(coding => fusion.fusionPartner3prime.flatMap(_.gene.hgncId).exists(_.value == coding.code.value))

        case None => false
      }
  }

  def RnaFusionOfInterest(
    params: Option[List[FusionParameters]],
    codings: Option[List[Coding[Gene.HgncId]]]
  ): RNAFusion => Boolean = {
    fusion =>
      RnaFusionOfInterestForParams(params)(fusion) || RnaFusionOfInterestForCodings(codings)(fusion)
  }


}

object VariantFilteringOps extends VariantFilteringOps
