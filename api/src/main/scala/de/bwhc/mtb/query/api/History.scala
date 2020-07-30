package de.bwhc.mtb.query.api


import java.time.Instant

import play.api.libs.json.Json



final case class History[T] private (
  snapshots: Seq[Snapshot[T]]
){

  def latest: Option[Snapshot[T]] =
    snapshots.headOption
}


object History
{

  def apply[T](
    snapshots: Seq[Snapshot[T]]
  ): History[T] =
    new History(
      snapshots.sortWith(
        (s1,s2) => s1.timestamp.isAfter(s2.timestamp)
      )
    )

}
