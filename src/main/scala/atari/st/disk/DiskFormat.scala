package atari.st.disk


abstract class DiskFormat(val size: Int)

case class StandardDiskFormat(tracks: Int, sectors: Int, sides: Int)
extends DiskFormat(tracks * sectors * sides * DiskFormat.sectorsPerTrack)

case class UnknownDiskFormat(override val size: Int)
extends DiskFormat(size)

object DiskFormat {

  val sectorsPerTrack = 512

  /* sensible max number of tracks when guessing format from size */
  protected val maxTracks = 83
  /* Possible number of sectors, in preference order */
  protected val probeSectors = List(9, 10, 11, 8, 12)
  /* Most sensible number of tracks, no specific order */
  protected val probeTracks = 78 to maxTracks

  def apply(tracks: Int, sectors: Int, sides: Int): StandardDiskFormat =
    StandardDiskFormat(tracks, sectors, sides)

  def apply(size: Int): DiskFormat = {
    /* 512 bytes per sector */
    if (size % 512 != 0) new UnknownDiskFormat(size)
    else {
      val size2 = size / 512
      /* First find possible sectors count */
      val candidates = probeSectors filter(size2 % _ == 0) map { sectors =>
        val size3 = size2 / sectors
        /* Consider single-sided if number of tracks is acceptable */
        if (size3 <= maxTracks) Some(DiskFormat(size3, sectors, 1))
        /* Consider double-sided if sensible */
        else if ((size3 % 2 == 0) && (size3 / 2 <= maxTracks)) Some(DiskFormat(size3 / 2, sectors, 2))
        /* Else drop this sectors count */
        else None
      }

      /* From possible candidates, first check if one has a sensible number of
       * tracks, otherwise get the first available format, else unknown. */
      candidates collectFirst {
        case Some(format) if (probeTracks.contains(format.tracks)) => format
      } orElse(candidates collectFirst {
        case Some(format) => format
      }) getOrElse(new UnknownDiskFormat(size))
    }
  }

}
