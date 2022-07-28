package de.bwhc.mtb.query.api


import play.api.libs.json.Json


final case class ResultSummary
(
  query: Query.Id,
  patientCount: Int,
  ngsReportCount: Int,
  therapyRecommendationCount: Int,
  therapyCount: Int,
  completionStats: Seq[QCReport.CompletionLevelWithFrequency]
)

object ResultSummary
{
  implicit val format = Json.format[ResultSummary]
}
