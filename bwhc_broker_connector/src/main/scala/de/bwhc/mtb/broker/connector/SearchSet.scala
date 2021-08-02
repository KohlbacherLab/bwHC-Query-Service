package de.bwhc.mtb.broker.connector


import play.api.libs.json.{Json,Reads}



case class SearchSet[T]
(
  entries: List[T]
)
{
  val total: Int = entries.size
}



object SearchSet
{

  implicit def searchSetReads[T](
    implicit t: Reads[T]
  ): Reads[SearchSet[T]] = Json.reads[SearchSet[T]]

}
