package atari.st

import atari.st.disk.{
  Disk,
  DiskFormat,
  DiskInfo,
  DiskType,
  StandardDiskFormat,
  UnknownDiskFormat
}
import atari.st.disk.exceptions.NoDiskInZipException
import atari.st.settings.Settings
import atari.st.util.Util
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.matching.Regex
import suiryc.scala.io.PathFinder
import suiryc.scala.io.NameFilter._


object DiskTools extends App {

  object AppMode extends Enumeration {
    val convert = Value
    val deduplicate = Value
    val inspect = Value
    val test = Value
  }

  case class AppOptions(
    allowByName: Boolean = false,
    checkBootSector: Boolean = Settings.core.checkBootSector,
    byName: Boolean = false,
    dryRun: Boolean = false,
    mode: AppMode.Value = AppMode.test,
    outputPreferred: Path = Settings.core.outputPreferred,
    outputOthers: Path = Settings.core.outputOthers,
    outputConverted: Path = Settings.core.outputConverted,
    outputType: DiskType.Value = DiskType.MSA,
    showDuplicates: Boolean = false,
    showUnique: Boolean = false,
    sources: List[Path] = Nil,
    verbose: Int = 0,
    warnUnknownFormat: Boolean = Settings.core.warnUnknownFormat,
    zipAllowDiskName: Boolean = Settings.core.zipAllowDiskName,
    zipAllowExtra: List[Regex] = Settings.core.zipAllowExtra,
    zipAllowExtraOverriden: Boolean = false,
    zipCharset: Charset = Settings.core.zipCharset
  )


  println(s"********** Started: ${new java.util.Date}")
  println(s"Args: ${args.toList}")

  val diskChecksums = mutable.Map[String, List[Disk]]()
  val diskNames = mutable.Map[String, List[Disk]]()

  val parser = new scopt.OptionParser[AppOptions]("TestST") {
    help("help") text("print help")
    opt[Unit]("check-boot-sector") text("check boot sector") action { (_, c) =>
      c.copy(checkBootSector = true)
    }
    opt[Unit]("no-check-boot-sector") text("do not check boot sector") action { (_, c) =>
      c.copy(checkBootSector = false)
    }
    opt[String]("output") text("output root path") action { (v, c) =>
      val outputRoot = Paths.get(v)
      c.copy(
        outputPreferred = outputRoot.resolve(Settings.core.outputRelativePreferred),
        outputOthers = outputRoot.resolve(Settings.core.outputRelativeOthers),
        outputConverted = outputRoot.resolve(Settings.core.outputRelativeConverted)
      )
    }
    opt[Unit]('v', "verbose") text("increase verbosity level") action { (_, c) =>
      c.copy(verbose = c.verbose + 1)
    }
    opt[Unit]("warn-unknown-format") text("warn on unknown disk format") action { (_, c) =>
      c.copy(warnUnknownFormat = true)
    }
    opt[Unit]("no-warn-unknown-format") text("do not warn on unknown disk format") action { (_, c) =>
      c.copy(warnUnknownFormat = false)
    }
    opt[Boolean]("zip-allow-disk-name") text("allow zip to contain another file which name is the disk") action { (v, c) =>
      c.copy(zipAllowDiskName = true)
    }
    opt[Boolean]("no-zip-allow-disk-name") text("do not allow zip to contain another file which name is the disk") action { (v, c) =>
      c.copy(zipAllowDiskName = false)
    }
    opt[String]("zip-allow-extra") text("allow extra zip entries name") unbounded() action { (v, c) =>
      val zipAllowExtra =
        if (!c.zipAllowExtraOverriden) List(v.r)
        else c.zipAllowExtra :+ v.r
      c.copy(zipAllowExtra = zipAllowExtra, zipAllowExtraOverriden = true)
    }
    val zipCharset = () => opt[String]("zip-charset") text("zip entries charset (if not UTF8)") minOccurs(
      if (Settings.core.zipCharset.compareTo(StandardCharsets.UTF_8) == 0) 1 else 0
    ) action { (v, c) =>
      c.copy(zipCharset = Charset.forName(v))
    }
    val source = () => arg[String]("source...") text("files/folders to process") unbounded() action { (v, c) =>
      c.copy(sources = c.sources :+ Paths.get(v))
    }

    cmd("convert") text("convert disk image") action { (_, c) =>
      c.copy(mode = AppMode.convert)
    } children(
      opt[String]("to") text("output type") action { (v, c) =>
        c.copy(outputType = DiskType(v))
      },
      zipCharset(),
      source()
    )

    cmd("deduplicate") text("deduplicate") action { (_, c) =>
      c.copy(mode = AppMode.deduplicate, byName = true)
    } children(
      opt[Unit]("allow-by-name") text("allow duplicates by name (but not by checksum)") action { (_, c) =>
        c.copy(allowByName = true)
      },
      opt[Unit]("dry-run") text("do not modify sources") action { (_, c) =>
        c.copy(dryRun = true)
      },
      zipCharset(),
      source()
    )

    cmd("inspect") text("inspect disks") action { (_, c) =>
      c.copy(mode = AppMode.inspect)
    } children(
      opt[Unit]("by-name") text("also show duplicates by name") action { (_, c) =>
        c.copy(byName = true)
      },
      opt[Unit]("show-duplicates") text("show duplicate disks") action { (_, c) =>
        c.copy(showDuplicates = true)
      },
      opt[Unit]("show-unique") text("show unique disks") action { (_, c) =>
        c.copy(showUnique = true)
      },
      zipCharset(),
      source()
    )

    cmd("test") text("test") action { (_, c) =>
      c.copy(mode = AppMode.test)
    }
  }

  val options = parser.parse(args, AppOptions()) getOrElse { sys.exit(1) }
  println(s"Options:")
  /* XXX - scala way to do this ? */
  for (field <- options.getClass().getDeclaredFields().toList.sortBy(_.getName())) {
    val fieldName = field.getName()
    val fieldValue = options.getClass().getMethod(fieldName).invoke(options)
    println(s"  ${fieldName}: ${fieldValue}")
  }
  implicit val charset = options.zipCharset

  options.mode match {
    case AppMode.convert =>
      convert()

    case AppMode.deduplicate =>
      deduplicate()

    case AppMode.inspect =>
      inspect()

    case AppMode.test =>
      test()
  }
  println(s"********** Ended: ${new java.util.Date}")

  def test() {
    for (sides <- 1 to 2) {
      for (tracks <- 78 to 84) {
        for (sectorsPerTrack <- 8 to 12) {
          val sectors = sectorsPerTrack * tracks * sides
          val format = DiskFormat(sectors, tracks, sectorsPerTrack, sides)
          val guessed = DiskFormat(format.size)
          if (format == guessed)
            println(s"${format.size / 1024}kiB: ${guessed}")
          else
            println(s"Wrong guess $guessed, expected format")
        }
      }
    }
  }

  def updateDuplicates(map: mutable.Map[String, List[Disk]], key: String, disk: Disk) {
    val duplicates = map.getOrElseUpdate(key, Nil)
    map.put(key, duplicates :+ disk)
  }

  def findDisks(): List[Disk] = {
    options.sources flatMap { _root =>
      val (root, files) =
        if (Files.isDirectory(_root)) {
          println(s"Searching for files in ${_root} ...")
          val finder = PathFinder(_root) ** """(?i).*\.(?:st|msa|zip)""".r
          val files = finder.get().toList
          println(s"Loading files info in ${_root} ...")
          (_root, files)
        }
        else if (Files.exists(_root)) {
          println(s"Loading file ${_root} info ...")
          (_root.getParent, List(_root.toFile))
        }
        else {
          println(s"Path ${_root} does not exist.")
          (_root, Nil)
        }
      files sortBy(_.toString.toLowerCase) map { file =>
        val path = file.toPath
        DiskInfo(path, options.zipAllowDiskName, options.zipAllowExtra).fold ({ ex =>
          ex match {
            case _: NoDiskInZipException =>

            case ex =>
              println(s"Error with file[$path]: ${ex.getMessage()}")
              ex.printStackTrace()
          }
          None
        }, { info =>
          Some(Disk(root, info))
        })
      } collect {
        case Some(v) => v
      }
    }
  }

  def findDuplicates() {
    val disks = findDisks()
    println("Searching duplicates ...")
    disks foreach { disk =>
      updateDuplicates(diskChecksums, disk.info.checksum, disk)
      if (options.byName)
        updateDuplicates(diskNames, disk.info.atomicName, disk)
    }
  }

  case class Duplicates(preferred: Disk, others: List[Disk], excluded: List[Disk])

  def sortDuplicates(duplicates: List[Disk]): Duplicates = {
    val pointsRef = scala.math.max(options.sources.length, 2)

    def pointsNameDepth(name: String) =
      pointsRef - name.split("/").length

    def pointsName(name: String, path: Path) = {
      val shortName = DiskInfo.atomicName(name)

      /* we prefer exact match */
      if (path.getFileName().toString.startsWith(s"${shortName}.")) pointsRef
      /* then case insensitive match */
      else if (path.getFileName().toString.toLowerCase.startsWith(s"${shortName.toLowerCase}.")) pointsRef / 2
      else 0
    }

    def pointsFileNameLength(path: Path) =
      path.getFileName().toString().length()

    def pointsPath(path: Path) =
      pointsRef - options.sources.map(path.startsWith(_)).zipWithIndex.find(_._1).map(_._2).getOrElse(pointsRef - 1) - 1

    def points(info: DiskInfo) =
      pointsNameDepth(info.name) + pointsName(info.name, info.path) + pointsPath(info.path)

    if (duplicates.length > 1) {
      /* sort by preferred format */
      val sorted = duplicates.sortBy(_.info.kind.id).reverse
      val sample1 = sorted.head.info
      /* drop less rich disk types if any */
      val (chosens, excluded) = sorted.partition(_.info.kind == sample1.kind)
      /* get the max points of duplicates */
      val sortedChosens = chosens.map(disk =>
        (disk, points(disk.info))
      ).sortBy(_._2).reverse
      /* and keep the max one, or settle for the one with the longest filename
       * (supposed to be more informational) */
      val preferredPoints = sortedChosens.head._2
      val preferred = sortedChosens.takeWhile(_._2 == preferredPoints).sortBy(tuple => pointsFileNameLength(tuple._1.info.path)).reverse.head._1
      val others = chosens.filterNot(_ eq preferred)

      Duplicates(preferred, others, excluded)
    }
    else Duplicates(duplicates.head, Nil, Nil)
  }

  def checkFormat(info: DiskInfo, force: Boolean = false) {
    info.format match {
      case format: UnknownDiskFormat =>
        if (options.warnUnknownFormat || options.checkBootSector || force)
          println(s"WARNING! Disk[${info.name}] image[${info.path}] has unknown format: ${format}")

      case format: StandardDiskFormat =>
        val bootSector = info.bootSector
        if (options.checkBootSector || force) {
          if ((bootSector.sectorsPerTrack != format.sectorsPerTrack) ||
            (bootSector.tracks != format.tracks) ||
            (bootSector.sides != format.sides))
          {
            println(s"WARNING! Disk[${info.name}] image[${info.path}] boot sector[${bootSector}] does not match format[${format}]")
          }
        }
    }
  }

  def inspect() {
    def inspect(map: mutable.Map[String, List[Disk]]) {
      map.toList sortBy(_._2.head.info.name.toLowerCase()) foreach { tuple =>
        val duplicates = sortDuplicates(tuple._2)
        val preferred = duplicates.preferred

        checkFormat(preferred.info)
        for (dup <- duplicates.others ::: duplicates.excluded)
          checkFormat(dup.info)

        if ((options.showDuplicates && (!duplicates.others.isEmpty || !duplicates.excluded.isEmpty)) ||
          options.showUnique)
        {
          if (duplicates.others.isEmpty && duplicates.excluded.isEmpty)
            println(s"Name: ${preferred.info.name}; Image: ${preferred.info}")
          else {
            println(s"Name: ${preferred.info.name}")
            println(s"  Preferred: ${preferred.info}")
          }
          if (!duplicates.others.isEmpty)
            println(s"  Duplicates: ${duplicates.others.map(_.info.path)}")
          if (!duplicates.excluded.isEmpty)
            println(s"  Excluded (for richer disk type): ${duplicates.excluded.map(_.info.path)}")
        }
      }
    }

    findDuplicates()

    if (options.byName) {
      println("")
      println("Disks by name")
      inspect(diskNames)
      println("")
    }

    println("")
    println("Disks by checksum")
    inspect(diskChecksums)
    println("")
  }

  def deduplicate() {
    def move(output: Path, root: Path, path: Path) {
      val target = Util.findTarget(output.resolve(root.relativize(path)))
      target.getParent.toFile.mkdirs()
      Files.move(path, target)
    }

    findDuplicates()

    diskChecksums.toList sortBy(_._2.head.info.name.toLowerCase()) foreach { tuple =>
      val duplicates = sortDuplicates(tuple._2)
      val preferred = duplicates.preferred

      val unsureDups =
        if (options.allowByName) Nil
        else {
          val disks = preferred :: duplicates.others ::: duplicates.excluded
          val names = disks.map(dup => dup.info.atomicName).toSet

          def isInDisks(disk: Disk) =
            disks.exists(_.info.path.compareTo(disk.info.path) == 0)

          names.toList flatMap { name =>
            diskNames(name) filterNot(isInDisks(_))
          } groupBy(_.info.path) map(_._2.head.info)
        }
      val unsure = !unsureDups.isEmpty

      checkFormat(preferred.info, unsure)
      for (dup <- duplicates.others ::: duplicates.excluded)
        checkFormat(dup.info, unsure)

      if (duplicates.others.isEmpty && duplicates.excluded.isEmpty && !unsure)
        println(s"Name: ${preferred.info.name}; Path: ${preferred.info.path}")
      else {
        println(s"Name: ${preferred.info.name}")
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

  def convert() {
    findDisks() foreach { disk =>
      checkFormat(disk.info)
      DiskConverter.convert(disk, options.outputConverted, options.outputType)
    }
  }

}
