package de.bwhc.mtb.query.impl


import java.time.LocalDateTime
import de.bwhc.mtb.dtos.ZPM
import de.bwhc.mtb.query.api.Query


final case class SavedQuery
(
  name: String,
  description: Option[String],
  query: Query,
  resultSet: Map[ZPM,List[ResultIds]],
  savedOn: LocalDateTime
)
{
  val id: Query.Id = query.id
}
