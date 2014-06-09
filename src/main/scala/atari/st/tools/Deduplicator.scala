package atari.st.tools

import atari.st.disk.{Disk, DuplicateStatus}
import atari.st.settings.Settings
import atari.st.util.Util
import java.nio.file.{Files, Path}


object Deduplicator {

  import Core._

  def deduplicate(inspect: Boolean) {
    def move(output: Path, root: Path, path: Path) {
      val target = Util.findTarget(output.resolve(root.relativize(path)))
      target.getParent.toFile.mkdirs()
      Files.move(path, target)
    }

    findDuplicates()

    diskChecksums.toList sortBy(_._2.preferred.info.normalizedName) foreach { tuple =>
      val checksum = tuple._1
      val duplicates = tuple._2
      val preferred = duplicates.preferred

      val (status, wouldKeep: Set[String], keptChecksums: Set[String], droppedChecksums: Set[String], unsureDups) =
        /* If duplicates by name are allowed, there is no unsure duplicate and
         * we keep this disk. */
        if (options.allowByName) (DuplicateStatus.keep, Set.empty, Set.empty, Set.empty, Nil)
        else {
          val disks = duplicates.disks
          val names = disks.map(_.info.normalizedName).toSet

          def isInDisks(disk: Disk) =
            disks.exists(_.info.checksum == disk.info.checksum)

          /* Get duplicates by name with different checksums. */
          val dups =
            names.toList flatMap { name =>
              diskNames(name).disks filterNot(isInDisks(_))
            } groupBy(_.info.path) map(_._2.head.info)

          Settings.core.duplicatesByName.get(checksum) map { dupsByName =>
            /* Checksum is known, check for actually unsure duplicates */
            val unsureDups = dups.filter { dup =>
              dupsByName.status(dup.checksum) == DuplicateStatus.unsure
            }
            val status =
              /* If there are unsure duplicates, our status is actually unsure */
              if (unsureDups.size > 0) DuplicateStatus.unsure
              /* Otherwise, get our configured status */
              else dupsByName.status(checksum)
            val dupsCkecksums = dups.map(_.checksum).toSet
            val kept = dupsByName.kept.filterNot(_ == checksum) & dupsCkecksums
            val dropped = dupsByName.dropped.filterNot(_ == checksum) & dupsCkecksums
            (status, dupsByName.kept, kept, dropped, unsureDups)
          } getOrElse {
            val status =
              /* If there are unsure duplicates, our status is actually unsure */
              if (dups.size > 0) DuplicateStatus.unsure
              /* Otherwise, by default we keep this disk */
              else DuplicateStatus.keep
            (status, Set.empty, Set.empty, Set.empty, dups)
          }
        }
      val unsure = (status == DuplicateStatus.unsure)

      checkFormat(preferred.info, unsure)
      for (dup <- duplicates.others ::: duplicates.excluded)
        checkFormat(dup.info, unsure)

      if (!inspect || (status != DuplicateStatus.keep) ||
        (options.showDuplicates && (!duplicates.others.isEmpty || !duplicates.excluded.isEmpty)) ||
        (options.showUnique && duplicates.others.isEmpty && duplicates.excluded.isEmpty))
      {
        if (duplicates.others.isEmpty && duplicates.excluded.isEmpty && (status == DuplicateStatus.keep))
          println(s"Name: ${preferred.info.normalizedName}; Image: ${preferred.info}")
        else {
          println(s"Name: ${preferred.info.normalizedName}")
          println(s"  Preferred: ${preferred.info}")
        }
        if (!duplicates.others.isEmpty)
          println(s"  Duplicates: ${duplicates.others.map(_.info.path)}")
        if (!duplicates.excluded.isEmpty)
          println(s"  Excluded (for richer disk type): ${duplicates.excluded.map(_.info.path)}")

        status match {
          case DuplicateStatus.keep =>
            val swith = if (!keptChecksums.isEmpty) s" with[${keptChecksums}]" else ""
            val sagainst = if (!droppedChecksums.isEmpty) s" unlike[${droppedChecksums}]" else ""
            if (!keptChecksums.isEmpty || !droppedChecksums.isEmpty)
              println(s"  Duplicate by name kept${swith}${sagainst}")
            if (!inspect && !options.dryRun) {
              move(options.outputPreferred, preferred.root, preferred.info.path)
              for (other <- duplicates.others ::: duplicates.excluded)
                move(options.outputOthers, other.root, other.info.path)
            }

          case DuplicateStatus.drop =>
            /* Sanity check: if we are configured to be dropped, but there is no
             * associated disk kept, then actually keep this disk.
             */
            val actuallyKeep = (keptChecksums.isEmpty && !wouldKeep.exists(diskChecksums.contains(_)))

            if (actuallyKeep)
              println(s"  Duplicate by name should be dropped, but actually kept due to missing preferred checksums[${wouldKeep}]")
            else if (keptChecksums.isEmpty)
              println(s"  Duplicate by name dropped in favor of other checksums (with different disk name): ${wouldKeep.find(diskChecksums.contains(_))}")
            else
              println(s"  Duplicate by name dropped in favor of other checksums: ${keptChecksums}")

            if (!inspect && !options.dryRun) {
              move(if (actuallyKeep) options.outputPreferred else options.outputOthers, preferred.root, preferred.info.path)
              for (other <- duplicates.others ::: duplicates.excluded)
                move(options.outputOthers, other.root, other.info.path)
            }

          case DuplicateStatus.unsure =>
            println(s"  No de-duplication due to unsure duplicates by name (but not by checksum): ${unsureDups}")
        }
      }
    }
  }

}
