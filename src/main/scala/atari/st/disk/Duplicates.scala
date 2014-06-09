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
  preferred: String,
  keep: Set[String],
  drop: Set[String]
) {

  def kept = keep

  def dropped = drop

  def status(checksum: String): DuplicateStatus.Value =
    if (keep.contains(checksum)) DuplicateStatus.keep
    else if (drop.contains(checksum)) DuplicateStatus.drop
    else DuplicateStatus.unsure

  def alternative(checksum: String): Boolean =
    if (checksum == preferred) false
    else status(checksum) match {
      case DuplicateStatus.keep => true
      case DuplicateStatus.drop => false
    }

}
