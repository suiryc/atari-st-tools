package atari.st.disk


case class Duplicates(preferred: Disk, others: List[Disk], excluded: List[Disk]) {
  lazy val disks = preferred :: others ::: excluded
}

object DuplicateStatus extends Enumeration {
  val keep = Value
  val drop = Value
  val unsure = Value
}

class ByNameDuplicates(
  val preferred: String,
  val kept: Set[String],
  val dropped: Set[String]
) {

  val known = kept ++ dropped

  def status(checksum: String): DuplicateStatus.Value =
    if (kept.contains(checksum)) DuplicateStatus.keep
    else if (dropped.contains(checksum)) DuplicateStatus.drop
    else DuplicateStatus.unsure

  def alternative(checksum: String): Boolean =
    if (checksum == preferred) false
    else status(checksum) match {
      case DuplicateStatus.keep => true
      case DuplicateStatus.drop => false
    }

}
