package de.bwhc.mtb.query.connector


import play.api.libs.json.{Json,Reads}
//import play.api.libs.json._



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

/*
    new Format[SearchSet[T]]{
      def writes(s: SearchSet[T]): JsValue = {
        Json.obj("entries" -> Json.toJson(s.entries),
                 "total"  -> s.total)
      }
      def reads(js: JsValue): JsResult[SearchSet[T]] = {
        (js \ "entries").validate[Iterable[T]].map(SearchSet(_))
      }
    }
*/
}
