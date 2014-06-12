package atari.st.tools

import atari.st.disk.{Disk, DiskInfo, Duplicates, DuplicateStatus}
import atari.st.settings.Settings
import atari.st.util.Util
import java.io.{BufferedOutputStream, ByteArrayInputStream, FileOutputStream}
import java.nio.file.{Files, Path}
import suiryc.scala.io.{FilesEx, IOStream, PathsEx}


object Deduplicator {

  import Core._

  case class DeduplicationInfo(
    /** Deduplication status */
    status: DuplicateStatus.Value,
    /** Whether this is an alternative disk */
    alternative: Boolean = false,
    /** Whether this is an alternative boot sector */
    alternativeBootSector: Boolean = false,
    /** By-name duplicates checksums that would be kept if present */
    wouldKeep: Set[String] = Set.empty,
    /** By-name duplicates that are actually kept */
    keptChecksums: Set[String] = Set.empty,
    /** By-name duplicates that are actually dropped */
    droppedChecksums: Set[String] = Set.empty,
    /** Unsure by-name duplicates */
    unsure: List[Disk] = Nil
  ) {

    def isUnsure =
      (status == DuplicateStatus.unsure)

  }

  def deduplicate(inspect: Boolean) {
    findDuplicates()

    diskChecksums.toList sortBy(_._2.preferred.info.normalizedName) foreach { tuple =>
      val checksum = tuple._1
      val duplicates = tuple._2
      val preferred = duplicates.preferred

      val dedupInfo = decideDuplicates(duplicates)
      /* If status is not 'unsure', then there is no unsure duplicates.
       * If there are unsure duplicates, then the status is 'unsure'.
       */

      checkFormat(preferred.info, dedupInfo.isUnsure)
      for (dup <- duplicates.others ::: duplicates.excluded)
        checkFormat(dup.info, dedupInfo.isUnsure)

      if (!inspect || (dedupInfo.status != DuplicateStatus.keep) ||
        (options.showDuplicates && (!duplicates.others.isEmpty || !duplicates.excluded.isEmpty)) ||
        (options.showUnique && duplicates.others.isEmpty && duplicates.excluded.isEmpty))
      {
        if (duplicates.others.isEmpty && duplicates.excluded.isEmpty && (dedupInfo.status == DuplicateStatus.keep))
          println(s"Name: ${preferred.info.normalizedName}; Image: ${preferred.info}")
        else {
          println(s"Name: ${preferred.info.normalizedName}")
          println(s"  Preferred: ${preferred.info}")
        }
        if (!duplicates.others.isEmpty)
          println(s"  Duplicates: ${duplicates.others.map(_.info.path)}")
        if (!duplicates.excluded.isEmpty)
          println(s"  Excluded (for richer disk type): ${duplicates.excluded.map(_.info.path)}")

        dedupInfo.status match {
          case DuplicateStatus.keep =>
            val swith = if (!dedupInfo.keptChecksums.isEmpty) s" with[${dedupInfo.keptChecksums}]" else ""
            val sagainst = if (!dedupInfo.droppedChecksums.isEmpty) s" unlike[${dedupInfo.droppedChecksums}]" else ""
            val salternative = if (dedupInfo.alternative) "alternative" else "preferred"
            if (!dedupInfo.keptChecksums.isEmpty || !dedupInfo.droppedChecksums.isEmpty || dedupInfo.alternative)
              println(s"  Duplicate by name kept (${salternative})${swith}${sagainst}")
            if (dedupInfo.alternativeBootSector) {
              if (!dedupInfo.alternative)
                println("  Duplicate with preferred boot sector kept")
              else
                println("  Duplicate with alternative boot sector kept")
            }
            if (!inspect)
              moveDuplicates(duplicates, dedupInfo)

          case DuplicateStatus.drop =>
            /* Sanity check: if we are configured to be dropped, but there is no
             * associated disk kept, then actually keep this disk.
             */
            val actuallyKeep = (dedupInfo.keptChecksums.isEmpty && !dedupInfo.wouldKeep.exists(diskChecksums.contains(_)))

            if (actuallyKeep)
              println(s"  Duplicate by name should be dropped, but actually kept due to missing preferred checksums[${dedupInfo.wouldKeep}]")
            else if (dedupInfo.keptChecksums.isEmpty)
              println(s"  Duplicate by name dropped in favor of other checksums (with different disk name): ${dedupInfo.wouldKeep.find(diskChecksums.contains(_))}")
            else
              println(s"  Duplicate by name dropped in favor of other checksums: ${dedupInfo.keptChecksums}")

            if (!inspect) {
              val actualDedupInfo = if (actuallyKeep) dedupInfo.copy(status = DuplicateStatus.keep) else dedupInfo
              moveDuplicates(duplicates, actualDedupInfo)
            }

          case DuplicateStatus.unsure =>
            println(s"  No de-duplication due to unsure duplicates by name (but not by checksum): ${dedupInfo.unsure.map(_.info)}")
        }
      }
    }
  }

  def decideDuplicates(duplicates: Duplicates): DeduplicationInfo = {
    val checksum = duplicates.preferred.info.checksum

    /* For each decision step, if there are unsure duplicates, our status is
     * actually unsure.
     */

    def decide: DeduplicationInfo =
      /* If duplicates by name are allowed, there is no unsure duplicate and
       * we keep this disk. */
      if (options.allowByName)
        DeduplicationInfo(DuplicateStatus.keep)
      else {
        val disks = duplicates.disks
        val names = disks.map(_.info.normalizedName).toSet

        def isInDisks(disk: Disk) =
          disks.exists(_.info.checksum == disk.info.checksum)

        /* Get duplicates by name with different checksums. */
        val unsure =
          names.toList flatMap { name =>
            diskNames(name).disks filterNot(isInDisks(_))
          } groupBy(_.info.path) map(_._2.head)

        if (unsure.size > 0)
          decideByChecksum(DeduplicationInfo(DuplicateStatus.unsure, unsure = unsure.toList))
        else
          DeduplicationInfo(DuplicateStatus.keep)
      }

    def decideByChecksum(dedupInfo: DeduplicationInfo): DeduplicationInfo =
      if (!dedupInfo.isUnsure) dedupInfo
      else Settings.core.duplicatesByName.get(checksum) map { dupsByName =>
        /* Checksum is known, check for actually unsure duplicates */
        val unsure =
          dedupInfo.unsure filterNot { dup =>
            /* First drop those in an 'alternative' folder */
            if (!options.duplicateAllowAlternatives) false
            else folderIsAlternative(dup)
          } filter { dup =>
            /* Then keep only those not known */
            dupsByName.status(dup.info.checksum) == DuplicateStatus.unsure
          } filterNot { dup =>
            /* Then drop those that actually are duplicates (only serial number
             * differs) of known disks. */
            if (!options.duplicateBootSectorAllow) false
            else (dupsByName.kept ++ dupsByName.dropped) exists { knownChecksum =>
              diskChecksums.get(knownChecksum) map { knownDuplicates =>
                (knownDuplicates.preferred.info.checksum2 == dup.info.checksum2) &&
                (knownDuplicates.preferred.info.bootSector.checksum == dup.info.bootSector.checksum)
              } getOrElse(false)
            }
          }

        if (unsure.size > 0) dedupInfo.copy(unsure = unsure)
        else {
          val dupsCkecksums = dedupInfo.unsure.map(_.info.checksum).toSet

          dedupInfo.copy(
            status = dupsByName.status(checksum),
            alternative = dupsByName.alternative(checksum),
            wouldKeep = dupsByName.kept,
            keptChecksums = dupsByName.kept.filterNot(_ == checksum) & dupsCkecksums,
            droppedChecksums = dupsByName.dropped.filterNot(_ == checksum) & dupsCkecksums,
            unsure = unsure
          )
        }
      } getOrElse(decideByChecksum2(dedupInfo))

    def decideByChecksum2(dedupInfo: DeduplicationInfo): DeduplicationInfo =
      if (!dedupInfo.isUnsure || !options.duplicateBootSectorAllow) dedupInfo
      else {
        val (sameChecksum, differentChecksum) =
          dedupInfo.unsure.partition(_.info.checksum2 == duplicates.preferred.info.checksum2)
        val (unsure, unsureAlternatives) =
          if (options.duplicateAllowAlternatives)
            differentChecksum.partition(!folderIsAlternative(_))
          else
            (differentChecksum, Nil)

        if (unsure.size > 0) {
          dedupInfo.copy(
            status = DuplicateStatus.unsure,
            unsure = unsure
          )
        }
        else {
          /* We need to sort ourself against previously unsure disks to know
           * whether we are the preferred one (to keep) or not.
           */
          val duplicates2 =
            sortDuplicates(duplicates.preferred :: sameChecksum, exclude = false)
          /* Group alternatives by actual boot sector checksum */
          val bootsectors = duplicates2.disks.groupBy(_.info.bootSector.checksum)
          /* We are an alternative boot sector (preferred or not) if there
           * actually are more than one different boot sector. */
          val alternativeBootSector = (bootsectors.size > 1)
          /* See if this boot sector is the preferred one or an alternative one */
          val alternative = !(duplicates2.preferred eq duplicates.preferred)
          val disks2 = bootsectors(duplicates.preferred.info.bootSector.checksum)
          if (disks2.head eq duplicates.preferred) {
            /* We are the preferred disk for this alternative boot sector */
            dedupInfo.copy(
              status = DuplicateStatus.keep,
              alternative = alternative,
              alternativeBootSector = alternativeBootSector,
              droppedChecksums = disks2.tail.map(_.info.checksum).toSet,
              unsure = unsure
            )
          }
          else {
            /* We are not the preferred disk for this alternative boot sector */
            dedupInfo.copy(
              status = DuplicateStatus.drop,
              alternative = alternative,
              alternativeBootSector = alternativeBootSector,
              keptChecksums = Set(disks2.head.info.checksum),
              unsure = unsure
            )
          }
        }
      }

    val dedupInfo = decide
    if (dedupInfo.isUnsure && options.duplicateAllowAlternatives && folderIsAlternative(duplicates.preferred)) {
      dedupInfo.copy(
        status = DuplicateStatus.keep,
        alternative = true
      )
    } else dedupInfo
  }

  def moveDuplicates(duplicates: Duplicates, dedupInfo: DeduplicationInfo) {
    def moveDisk(disk: Disk) {
      val output = dedupInfo.status match {
        case DuplicateStatus.drop =>
          options.outputOthers

        case DuplicateStatus.keep =>
          if (disk eq duplicates.preferred) options.outputPreferred
          else options.outputOthers
      }

      def getAlternativesFolder(path: Path) =
        if (folderIsAlternative(path.getParent)) path
        else path.getParent.resolve(Settings.core.outputRelativeAlternatives).resolve(path.getFileName)

      val target0 = output.resolve(disk.root.relativize(disk.info.path))
      val target1 =
        if (dedupInfo.alternative) {
          if (dedupInfo.alternativeBootSector) {
            options.duplicateBootSectorAlternativeImage map { suffix =>
              if (folderIsAlternative(target0.getParent)) target0
              else target0.getParent.resolve(s"${Settings.core.outputRelativeAlternatives}.${suffix}").resolve(target0.getFileName)
            } getOrElse(getAlternativesFolder(options.outputOthers.resolve(disk.root.relativize(disk.info.path))))
          }
          else getAlternativesFolder(target0)
        }
        else target0
      val target = Util.findTarget(target1)

      if (!options.dryRun) {
        target.getParent.toFile.mkdirs()
        Files.move(disk.info.path, target)
      }
      if (options.verbose > 1)
        println(s"Moved ${disk.info.path} to ${target}")

      if (dedupInfo.alternativeBootSector && dedupInfo.alternative)
        options.duplicateBootSectorAlternativeSector foreach { suffix =>
          val bsName = s"${PathsEx.atomicName(disk.info.path)}.${suffix}"
          val target = output.resolve(disk.root.relativize(disk.info.path.resolveSibling(bsName)))

          if (!options.dryRun) {
            target.getParent.toFile.mkdirs()
            val input = new ByteArrayInputStream(disk.info.bootSector.data)
            val output = new BufferedOutputStream(new FileOutputStream(target.toFile))
            IOStream.transfer(input, output)
            output.flush()
            output.close()
            FilesEx.setTimes(target, disk.info.times)
          }
          if (options.verbose > 1)
            println(s"Saved ${disk.info.path} boot sector to ${target}")
        }

      if (!options.dryRun) {
        def delete(path: Path) {
          if ((path.compareTo(disk.root) != 0) && path.toFile.delete)
            delete(path.getParent)
        }

        delete(disk.info.path.getParent)
      }
    }

    duplicates.disks foreach(moveDisk)
  }

}
