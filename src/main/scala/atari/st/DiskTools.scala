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
    checkBootSector: Boolean = false,
    byName: Boolean = false,
    dryRun: Boolean = false,
    mode: AppMode.Value = AppMode.test,
    output: Path = null,
    outputType: DiskType.Value = DiskType.MSA,
    showDuplicates: Boolean = false,
    showUnique: Boolean = false,
    sources: List[Path] = Nil,
    verbose: Int = 0,
    warnUnknownFormat: Boolean = false,
    zipAllowedExtra: Option[Regex] = None,
    zipCharset: Charset = Charset.defaultCharset()
  )


  println(s"********** Started: ${new java.util.Date}")
  println(s"Args: ${args.toList}")
  println(s"Default charset: ${Charset.defaultCharset()}")

  val diskChecksums = mutable.Map[String, List[Disk]]()
  val diskNames = mutable.Map[String, List[Disk]]()

  val parser = new scopt.OptionParser[AppOptions]("TestST") {
    help("help") text("print help")
    val zipCharset = () => opt[Unit]("check-boot-sector") text("check boot sector") action { (_, c) =>
      c.copy(checkBootSector = true)
    }
    opt[Unit]('v', "verbose") text("increase verbosity level") action { (_, c) =>
      c.copy(verbose = c.verbose + 1)
    }
    opt[Unit]("warn-unknown-format") text("warn on unknown disk format") action { (_, c) =>
      c.copy(warnUnknownFormat = true)
    }
    opt[String]("zip-allowed-extra") text("allowed extra zip entries name") action { (v, c) =>
      c.copy(zipAllowedExtra = Some(v.r))
    }
    val checkBootSector = () => opt[String]("zip-charset") text("zip entries charset (if not UTF8)") minOccurs(
      if (Charset.defaultCharset().compareTo(StandardCharsets.UTF_8) == 0) 1 else 0
    ) action { (v, c) =>
      c.copy(zipCharset = Charset.forName(v))
    }
    val output = () => opt[String]("output") text("output path") required() action { (v, c) =>
      c.copy(output = Paths.get(v))
    }
    val source = () => arg[String]("source...") text("files/folders to process") unbounded() action { (v, c) =>
      c.copy(sources = c.sources :+ Paths.get(v))
    }

    cmd("convert") text("convert disk image") action { (_, c) =>
      c.copy(mode = AppMode.convert)
    } children(
      checkBootSector(),
      output(),
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
      checkBootSector(),
      opt[Unit]("dry-run") text("do not modify sources") action { (_, c) =>
        c.copy(dryRun = true)
      },
      output(),
      zipCharset(),
      source()
    )

    cmd("inspect") text("inspect disks") action { (_, c) =>
      c.copy(mode = AppMode.inspect)
    } children(
      opt[Unit]("by-name") text("also show duplicates by name") action { (_, c) =>
        c.copy(byName = true)
      },
      checkBootSector(),
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

  def atomicName(name: String) =
    /* keep filename */
    name.split("/").toList.reverse.head.
      /* without extension */
      split("""\.""").reverse.tail.reverse.mkString(".")

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
        DiskInfo(path, options.zipAllowedExtra).fold ({ ex =>
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
        updateDuplicates(diskNames, atomicName(disk.info.name.toLowerCase()), disk)
    }
  }

  case class Duplicates(preferred: Disk, others: List[Disk], excluded: List[Disk])

  def sortDuplicates(duplicates: List[Disk]): Duplicates = {
    val pointsRef = scala.math.max(options.sources.length, 2)

    def pointsNameDepth(name: String) =
      pointsRef - name.split("/").length

    def pointsName(name: String, path: Path) = {
      val shortName = atomicName(name)

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

  def checkFormat(info: DiskInfo) {
    info.format match {
      case format: UnknownDiskFormat =>
        if (options.warnUnknownFormat || options.checkBootSector)
          println(s"WARNING! Disk[${info.name}] image[${info.path}] has unknown format: ${format}")

      case format: StandardDiskFormat =>
        val bootSector = info.bootSector
        if (options.checkBootSector) {
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
    def move(folder: String, root: Path, path: Path) {
      val target = Util.findTarget(options.output.resolve(folder).resolve(root.relativize(path)))
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
          val names = disks.map(dup => atomicName(dup.info.name.toLowerCase())).toSet

          def isInDisks(disk: Disk) =
            disks.exists(_.info.path.compareTo(disk.info.path) == 0)

          names.toList flatMap { name =>
            diskNames(name) filterNot(isInDisks(_))
          } groupBy(_.info.path) map(_._2.head.info)
        }

      checkFormat(preferred.info)
      for (dup <- duplicates.others ::: duplicates.excluded)
        checkFormat(dup.info)

      if (duplicates.others.isEmpty && duplicates.excluded.isEmpty && unsureDups.isEmpty)
        println(s"Name: ${preferred.info.name}; Path: ${preferred.info.path}")
      else {
        println(s"Name: ${preferred.info.name}")
        println(s"  Preferred: ${preferred.info}")
      }
      if (!duplicates.others.isEmpty)
        println(s"  Duplicates: ${duplicates.others.map(_.info.path)}")
      if (!duplicates.excluded.isEmpty)
        println(s"  Excluded (for richer disk type): ${duplicates.excluded.map(_.info.path)}")

      if (!unsureDups.isEmpty)
        println(s"  No de-duplication due to unsure duplicates by name (but not by checksum): ${unsureDups}")
      else {
        if (!options.dryRun) {
          move("preferred", preferred.root, preferred.info.path)
          for (other <- duplicates.others ::: duplicates.excluded)
            move("others", other.root, other.info.path)
        }
      }
    }
  }

  def convert() {
    findDisks() foreach { disk =>
      checkFormat(disk.info)
      DiskConverter.convert(disk, options.output, options.outputType)
    }
  }

}
