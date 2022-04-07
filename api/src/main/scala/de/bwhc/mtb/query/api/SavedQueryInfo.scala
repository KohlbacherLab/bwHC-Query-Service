package de.bwhc.mtb.query.api


import java.time.LocalDateTime


final case class SavedQueryInfo
(
  id: Query.Id,
  name: String,
  description: Option[String],
  savedOn: LocalDateTime,
  cohortSize: Int
)
