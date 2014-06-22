package atari.st.tools

import atari.st.disk.{Disk, DiskInfo, Duplicates, DuplicateStatus}
import atari.st.settings.Settings
import atari.st.util.Util
import java.io.{BufferedOutputStream, ByteArrayInputStream, FileOutputStream}
import java.nio.file.{Files, Path}
import scala.collection.mutable
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
    /** Original disk, if known */
    original: Option[Disk] = None,
    /** By-name duplicates checksums that would be kept if present */
    wouldKeep: Set[String] = Set.empty,
    /** By-name duplicates that are actually kept */
    keptChecksums: Set[String] = Set.empty,
    /** By-name duplicates that are actually dropped */
    droppedChecksums: Set[String] = Set.empty,
    /** Unsure by-name duplicates */
    unsure: List[Disk] = Nil
  ) {

    /* Various cases:
     *  - !alternative && !alternativeBootSector: preferred disk
     *    => move to target
     *  - alternative && !alternativeBootSector: alternative disk
     *    => move to target 'alternatives' if not already in an 'alternative' folder
     *  - !alternative && alternativeBootSector: preferred (boot sector) disk
     *    => move to target
     *  - alternative && alternativeBootSector: alternative (boot sector) disk
     *    => move to target 'alternatives.boot-sector' if allowed and not already in an 'alternative' folder
     *    => save boot sector to target as 'diskname.alternative.bs' if requested
     * Target is the 'preferred' folder if disk is kept, 'others' if dropped.
     * If status is unsure, disk is not moved.
     */

    def isUnsure =
      (status == DuplicateStatus.unsure)

  }

  /* Already decided duplicates (by checksum). */
  val decided = mutable.Map[String, DeduplicationInfo]()

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
            if (dedupInfo.alternative || dedupInfo.alternativeBootSector)
              dedupInfo.original foreach { disk =>
                if (!(disk eq preferred))
                  println(s"  Original: ${disk.info}")
              }
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

    /* Note: scala compiler does not seem to like 'foreach' inside 'map' etc. */
    def decide() {
      if (!decided.contains(checksum)) {
        /* If duplicates by name are allowed, there is no unsure duplicate and
         * we keep this disk. */
        if (options.allowByName)
          decided(checksum) = DeduplicationInfo(DuplicateStatus.keep)
        else {
          val disks = duplicates.disks
          val names = disks.map(_.info.normalizedName).toSet
          val checksums = disks.map(_.info.checksum).toSet

          /* Get duplicates by name with different checksums. */
          val unsureChecksums =
            names.flatMap(name =>
              diskNames(name).map(_.info.checksum)
            ) -- checksums
          val unsure =
            unsureChecksums.toList.map(diskChecksums(_).preferred)

          if (unsure.size > 0)
            decideByChecksum(DeduplicationInfo(DuplicateStatus.unsure, unsure = unsure))
          else
            decided(checksum) = DeduplicationInfo(DuplicateStatus.keep)
        }
      }
      /* else: already decided */
    }

    def decideByChecksum(dedupInfo: DeduplicationInfo) {
      /* Check if at least one checksum from the duplicates is known */
      val checksums = checksum ::
        (dedupInfo.unsure.map(_.info.checksum).toSet - checksum).toList
      checksums.toStream.map(Settings.core.duplicatesByName.get(_)).collect {
        case Some(v) => v
      }.headOption match {
        case None =>
          decideByChecksum2(dedupInfo)

        case Some(dupsByName) =>
          val disks = duplicates.preferred :: dedupInfo.unsure
          val (knownChecksums, unknownChecksums) =
            disks partition { disk =>
              dupsByName.status(disk.info.checksum) != DuplicateStatus.unsure
            }
          val (sameImages, otherImages) =
            unknownChecksums partition { disk =>
              /* Check images that actually are duplicates (only serial number
               * differs) of known disks. */
              if (!options.duplicateBootSectorAllow) false
              else dupsByName.known exists { knownChecksum =>
                diskChecksums.get(knownChecksum) map { knownDuplicates =>
                  (knownDuplicates.preferred.info.checksum2 == disk.info.checksum2) &&
                  (knownDuplicates.preferred.info.bootSector.checksum == disk.info.bootSector.checksum)
                } getOrElse(false)
              }
            }
          val (unsureAlternatives, unsure) =
            if (options.duplicateAllowAlternatives)
              otherImages.partition(folderIsAlternative)
            else
              (Nil, otherImages)

          if (unsure.size > 0) {
            val diskDedupInfo = dedupInfo.copy(unsure = unsure)
            disks foreach { disk =>
              val diskChecksum = disk.info.checksum
              decided(diskChecksum) = diskDedupInfo
            }
          }
          else {
            val checksums = disks.map(_.info.checksum).toSet
            val keptChecksums = dupsByName.kept ++ unsureAlternatives.map(_.info.checksum)
            val droppedChecksums = dupsByName.dropped
            val original = diskChecksums.get(dupsByName.preferred) map(_.preferred)

            /* We got 3 list of disks:
             *  1. Known checksums: decided by configuration
             */
            knownChecksums foreach { disk =>
              val diskChecksum = disk.info.checksum
              val diskDedupInfo =
                dedupInfo.copy(
                  status = dupsByName.status(diskChecksum),
                  alternative = dupsByName.alternative(diskChecksum),
                  original = original,
                  wouldKeep = dupsByName.kept,
                  keptChecksums = keptChecksums.filterNot(_ == diskChecksum) & checksums,
                  droppedChecksums = droppedChecksums.filterNot(_ == diskChecksum) & checksums,
                  unsure = unsure
                )
              decided(diskChecksum) = diskDedupInfo
            }

            /*  2. Images that actually have the same checksum than known ones: we
             *     drop those in favor of the configured ones.
             */
            sameImages foreach { disk =>
              val diskChecksum = disk.info.checksum
              val diskDedupInfo =
                dedupInfo.copy(
                  status = DuplicateStatus.drop,
                  wouldKeep = dupsByName.kept,
                  keptChecksums = keptChecksums.filterNot(_ == diskChecksum) & checksums,
                  droppedChecksums = droppedChecksums.filterNot(_ == diskChecksum) & checksums,
                  unsure = unsure
                )
              decided(diskChecksum) = diskDedupInfo
            }

            /* 3. Images that do not match but are in 'alternative' folders: we
             *    keep those as alternatives.
             */
            unsureAlternatives foreach { disk =>
              val diskChecksum = disk.info.checksum
              val diskDedupInfo =
                dedupInfo.copy(
                  status = DuplicateStatus.keep,
                  alternative = true,
                  original = original,
                  keptChecksums = keptChecksums.filterNot(_ == diskChecksum) & checksums,
                  unsure = unsure
                )
              decided(diskChecksum) = diskDedupInfo
            }
          }
      }
    }

    def decideByChecksum2(dedupInfo: DeduplicationInfo) {
      val disks = duplicates.preferred :: dedupInfo.unsure
      val (sameChecksum, differentChecksum) =
        if (options.duplicateBootSectorAllow)
          dedupInfo.unsure.partition(_.info.checksum2 == duplicates.preferred.info.checksum2)
        else
          (Nil, dedupInfo.unsure)
      val (unsureAlternatives, unsure) =
        if (!options.duplicateBootSectorAllow)
          (Nil, dedupInfo.unsure)
        else if (options.duplicateAllowAlternatives)
          differentChecksum.partition(folderIsAlternative)
        else
          (Nil, differentChecksum)

      if (unsure.size > 0) {
        if (options.duplicateAllowAlternatives && (unsure.size == 1) && folderIsAlternative(duplicates.preferred)) {
          /* We are an 'alternative' and there is actually only one 'preferred'
           * disk. */
          val duplicatesAll = sortDuplicates(disks, exclude = false)
          val keptChecksums = disks.map(_.info.checksum).toSet
          val original = Some(duplicatesAll.preferred)

          disks foreach { disk =>
            val diskChecksum = disk.info.checksum
            val diskDedupInfo =
              dedupInfo.copy(
                status = DuplicateStatus.keep,
                alternative = (diskChecksum != duplicatesAll.preferred.info.checksum),
                original = original,
                keptChecksums = keptChecksums.filterNot(_ == diskChecksum),
                unsure = Nil
              )
            decided(diskChecksum) = diskDedupInfo
          }
        }
        else {
          val diskDedupInfo = dedupInfo.copy(unsure = unsure)
          disks foreach { disk =>
            val diskChecksum = disk.info.checksum
            decided(diskChecksum) = diskDedupInfo
          }
        }
      }
      else {
        /* We need to sort ourself against previously unsure disks to know
         * whether we are the preferred one (to keep) or not.
         */
        val duplicatesSame =
          sortDuplicates(duplicates.preferred :: sameChecksum, exclude = false)
        val duplicatesAll = sortDuplicates(disks, exclude = false)

        /* Group boot sector alternatives by actual boot sector checksum.
         * Only the first of each group is kept.
         */
        val bootsectors = duplicatesSame.disks.groupBy(_.info.bootSector.checksum)
        val keptChecksums = bootsectors.map(_._2.head.info.checksum).toSet ++ unsureAlternatives.map(_.info.checksum)
        val droppedChecksums = bootsectors.flatMap(_._2.tail).map(_.info.checksum).toSet
        val original = Some(duplicatesAll.preferred)

        if (!(duplicatesSame.preferred eq duplicatesAll.preferred)) {
          /* Special case: if one of the 'alternative' images is preferred,
           * then it becomes the overall preferred one, and we become 'simple'
           * alternatives.
           */
          disks foreach { disk =>
            val diskChecksum = disk.info.checksum
            val diskDedupInfo =
              dedupInfo.copy(
                status = if (keptChecksums.contains(diskChecksum)) DuplicateStatus.keep else DuplicateStatus.drop,
                alternative = (diskChecksum != duplicatesAll.preferred.info.checksum),
                original = original,
                keptChecksums = keptChecksums.filterNot(_ == diskChecksum),
                droppedChecksums = droppedChecksums.filterNot(_ == diskChecksum),
                unsure = unsure
              )
            decided(diskChecksum) = diskDedupInfo
          }
        }
        else {
          /* We are an alternative boot sector (preferred or not) if there
           * actually are more than one boot sector. */
          val alternativeBootSector = (bootsectors.size > 1)

          /* We got 2 lists of disks:
           *  1. Images with same content checksum and only different boot
           *     sectors: we keep the first disk of each different boot sector
           *     group, and the preferred disk is the preferred boot sector
           */
          duplicatesSame.disks foreach { disk =>
            val diskChecksum = disk.info.checksum
            val diskDedupInfo =
              dedupInfo.copy(
                status = if (keptChecksums.contains(diskChecksum)) DuplicateStatus.keep else DuplicateStatus.drop,
                alternative = (diskChecksum != duplicatesSame.preferred.info.checksum),
                alternativeBootSector = alternativeBootSector,
                original = original,
                keptChecksums = keptChecksums.filterNot(_ == diskChecksum),
                droppedChecksums = droppedChecksums.filterNot(_ == diskChecksum),
                unsure = unsure
              )
            decided(diskChecksum) = diskDedupInfo
          }

          /* 2. Images that do not match but are in 'alternative' folders: we
           *    keep/drop those as alternatives.
           */
          unsureAlternatives foreach { disk =>
            val diskChecksum = disk.info.checksum
            val diskDedupInfo =
              dedupInfo.copy(
                status = DuplicateStatus.keep,
                alternative = true,
                alternativeBootSector = false,
                original = original,
                keptChecksums = keptChecksums.filterNot(_ == diskChecksum),
                droppedChecksums = droppedChecksums.filterNot(_ == diskChecksum),
                unsure = unsure
              )
            decided(diskChecksum) = diskDedupInfo
          }
        }
      }
    }

    decide()

    /* We have now decided */
    decided(checksum)
  }

  def moveDuplicates(duplicates: Duplicates, dedupInfo: DeduplicationInfo) {
    def moveDisk(disk: Disk) {
      val output = dedupInfo.status match {
        case DuplicateStatus.drop =>
          options.outputOthers

        case DuplicateStatus.keep =>
          val canKeep =
            !dedupInfo.alternative ||
            !dedupInfo.alternativeBootSector ||
            options.duplicateBootSectorAlternativeImage.isDefined
          if ((disk eq duplicates.preferred) && canKeep) options.outputPreferred
          else options.outputOthers
      }

      def getNominalTarget(path: Path) =
        if (!folderIsAlternative(path.getParent)) path
        else path.getParent.resolveSibling(path.getFileName)

      val targetRelative0 = disk.root.relativize(disk.info.path)
      val originalDisk = dedupInfo.original getOrElse(disk)
      /* We may need to move this disk relatively to the original disk */
      val originalTargetRelative =
        getNominalTarget(originalDisk.root.relativize(originalDisk.info.path))

      def getAlternativeTarget(alternativeName: String) =
        if (folderIsAlternative(targetRelative0.getParent))
          /* Keep 'alternative' folder relative to original disk */
          Option(targetRelative0.getParent.getParent) map { grandparent =>
            originalTargetRelative.resolveSibling(grandparent.relativize(targetRelative0))
          } getOrElse(originalTargetRelative.resolveSibling(targetRelative0))
        else
          /* Create 'alternative' boot sector folder relatively to original disk */
          originalTargetRelative.resolveSibling(alternativeName).resolve(targetRelative0.getFileName)

      val targetRelative =
        if (dedupInfo.alternative) {
          if (dedupInfo.alternativeBootSector && options.duplicateBootSectorAlternativeImage.isDefined)
            getAlternativeTarget(s"${Settings.core.outputRelativeAlternatives}.${options.duplicateBootSectorAlternativeImage.get}")
          else
            getAlternativeTarget(Settings.core.outputRelativeAlternatives)
        }
        else getNominalTarget(targetRelative0)
      val target = Util.findTarget(output.resolve(targetRelative))

      if (!options.dryRun) {
        target.getParent.toFile.mkdirs()
        Files.move(disk.info.path, target)
      }
      if (options.verbose > 1)
        println(s"Moved ${disk.info.path} to ${target}")

      if (dedupInfo.alternativeBootSector && dedupInfo.alternative)
        options.duplicateBootSectorAlternativeSector foreach { suffix =>
          val bsName = s"${PathsEx.atomicName(originalDisk.info.path)}.${suffix}"
          val target = output.resolve(originalTargetRelative.resolveSibling(bsName))

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
