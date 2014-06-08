package atari.st.tools

import atari.st.disk.Disk
import atari.st.util.Util
import java.nio.file.{Files, Path}


object Deduplicator {

  import Core._

  def deduplicate() {
    def move(output: Path, root: Path, path: Path) {
      val target = Util.findTarget(output.resolve(root.relativize(path)))
      target.getParent.toFile.mkdirs()
      Files.move(path, target)
    }

    findDuplicates()

    diskChecksums.toList sortBy(_._2.preferred.info.normalizedName) foreach { tuple =>
      val duplicates = tuple._2
      val preferred = duplicates.preferred

      val unsureDups =
        if (options.allowByName) Nil
        else {
          val disks = duplicates.disks
          val names = disks.map(dup => dup.info.normalizedName).toSet

          def isInDisks(disk: Disk) =
            disks.exists(_.info.path.compareTo(disk.info.path) == 0)

          names.toList flatMap { name =>
            diskNames(name).disks filterNot(isInDisks(_))
          } groupBy(_.info.path) map(_._2.head.info)
        }
      val unsure = !unsureDups.isEmpty

      checkFormat(preferred.info, unsure)
      for (dup <- duplicates.others ::: duplicates.excluded)
        checkFormat(dup.info, unsure)

      if (duplicates.others.isEmpty && duplicates.excluded.isEmpty && !unsure)
        println(s"Name: ${preferred.info.normalizedName}; Path: ${preferred.info.path}")
      else {
        println(s"Name: ${preferred.info.normalizedName}")
        println(s"  Preferred: ${preferred.info}")
      }
      if (!duplicates.others.isEmpty)
        println(s"  Duplicates: ${duplicates.others.map(_.info.path)}")
      if (!duplicates.excluded.isEmpty)
        println(s"  Excluded (for richer disk type): ${duplicates.excluded.map(_.info.path)}")

      if (unsure)
        println(s"  No de-duplication due to unsure duplicates by name (but not by checksum): ${unsureDups}")
      else {
        if (!options.dryRun) {
          move(options.outputPreferred, preferred.root, preferred.info.path)
          for (other <- duplicates.others ::: duplicates.excluded)
            move(options.outputOthers, other.root, other.info.path)
        }
      }
    }
  }

}
