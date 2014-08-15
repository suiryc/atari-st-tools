package atari.st.disk


abstract class DiskFormat(val size: Int)

case class StandardDiskFormat(sectors: Int, tracks: Int, sectorsPerTrack: Int, sides: Int)
extends DiskFormat(sectors * DiskFormat.bytesPerSector)

case class UnknownDiskFormat(override val size: Int)
extends DiskFormat(size)

object DiskFormat {

  val bytesPerSector = 512

  /* sensible max number of tracks when guessing format from size */
  protected val maxTracks = 84
  /* Possible number of sectors per track, in preference order */
  protected val probeSectorsPerTrack = List(9, 10, 11, 8, 12)
  /* Most sensible number of tracks, no specific order */
  protected val probeTracks = 78 to maxTracks

  def apply(sectors: Int, tracks: Int, sectorsPerTrack: Int, sides: Int): StandardDiskFormat =
    StandardDiskFormat(sectors, tracks, sectorsPerTrack, sides)

  def apply(bootSector: BootSector, size: Int): DiskFormat =
    if (bootSector.sectors * DiskFormat.bytesPerSector != size) {
      /* Either boot sector has wrong info, or image is not complete.
       * In both cases, try to guess.
       */
      DiskFormat(size)
    }
    else {
      DiskFormat(bootSector.sectors, bootSector.tracks, bootSector.sectorsPerTrack, bootSector.sides)
    }

  def apply(size: Int): DiskFormat = {
    if (size % bytesPerSector != 0) new UnknownDiskFormat(size)
    else {
      val sectors = size / bytesPerSector
      /* First find possible sectors per track */
      val candidates = probeSectorsPerTrack filter(sectors % _ == 0) map { sectorsPerTrack =>
        val tracks = sectors / sectorsPerTrack
        /* Consider single-sided if number of tracks is acceptable */
        if (tracks <= maxTracks) Some(DiskFormat(sectors, tracks, sectorsPerTrack, 1))
        /* Consider double-sided if sensible */
        else if ((tracks % 2 == 0) && (tracks / 2 <= maxTracks)) Some(DiskFormat(sectors, tracks / 2, sectorsPerTrack, 2))
        /* Else drop this sectors count */
        else None
      }

      /* From possible candidates, first check if one has a sensible number of
       * tracks, otherwise get the first available format, else unknown. */
      candidates.collectFirst {
        case Some(format) if (probeTracks.contains(format.tracks)) => format
      }.orElse(candidates.collectFirst {
        case Some(format) => format
      }).getOrElse(new UnknownDiskFormat(size))
    }
  }

}
