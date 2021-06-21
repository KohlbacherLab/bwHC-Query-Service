package de.bwhc.mtb.query.api


import play.api.libs.json.Json


final case class ResultSummary
(
  query: Query.Id,
  patientTotal: Int,
  completionStats: Seq[QCReport.CompletionLevelWithFrequency]
/*
  ngsReports: Int,
  therapyRecommendations: Int,
  therapies: Int  
*/
)

object ResultSummary
{
  implicit val format = Json.format[ResultSummary]
}
