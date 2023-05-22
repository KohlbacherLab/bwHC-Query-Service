package de.bwhc.mtb.query.api



import de.bwhc.mtb.data.entry.views.{
  SimpleVariantView,
  CNVView,
  DNAFusionView,
  RNAFusionView
}
import play.api.libs.json.Json



final case class VariantsOfInterest
(
  query: Query.Id,
  simpleVariants: Iterable[SimpleVariantView],
  copyNumberVariants: Iterable[CNVView],
  dnaFusions: Iterable[DNAFusionView],
  rnaFusions: Iterable[RNAFusionView]
)


object VariantsOfInterest
{
  implicit val writes = Json.writes[VariantsOfInterest]
}
