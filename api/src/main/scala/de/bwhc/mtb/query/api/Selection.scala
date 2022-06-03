package de.bwhc.mtb.query.api



import play.api.libs.json.{Json,Format}


final case class Selection[T]
(
  name: String,
  items: Seq[Selection.Item[T]]
){

  def unselect(ts: T*): Selection[T] = {
    this.copy(
      items = items.map {
        case item @ Selection.Item(t,_) => item.copy(selected = !ts.contains(t))
      }
    )
  }

  def selectOnly(ts: T*): Selection[T] = {
    this.copy(
      items = items.map {
        case item @ Selection.Item(t,_) => item.copy(selected = ts.contains(t))
      }
    )
  }

  def selectedValues: Seq[T] =
    items.filter(_.selected).map(_.value)

  def isSelected(t: T): Boolean =
    items.exists(item => item.value == t && item.selected)

}

object Selection
{

  final case class Item[T]
  (
    value: T,
    selected: Boolean
  )


  implicit def formatItem[T: Format] = Json.format[Item[T]]
  implicit def format[T: Format] = Json.format[Selection[T]]

}
