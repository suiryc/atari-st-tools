package atari.st

import atari.st.disk.{
  DiskFormat,
  DiskInfo,
  NoDiskInZipException,
  UnknownDiskFormat
}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.matching.Regex
import suiryc.scala.io.PathFinder
import suiryc.scala.io.NameFilter._


object DiskTools extends App {

  object AppMode extends Enumeration {
    val test = Value
    val showDuplicates = Value
    val deduplicate = Value
  }

  case class AppOptions(
    allowByName: Boolean = false,
    byName: Boolean = false,
    dryRun: Boolean = false,
    mode: AppMode.Value = AppMode.test,
    output: Path = null,
    sources: List[Path] = Nil,
    verbose: Int = 0,
    warnUnknownFormat: Boolean = false,
    zipAllowedExtra: Option[Regex] = None,
    zipCharset: Charset = Charset.defaultCharset()
  )

  case class Disk(root: Path, info: DiskInfo)


  println(s"********** Started: ${new java.util.Date}")
  println(s"Args: ${args.toList}")
  println(s"Default charset: ${Charset.defaultCharset()}")

  val diskChecksums = mutable.Map[String, List[Disk]]()
  val diskNames = mutable.Map[String, List[Disk]]()

  val parser = new scopt.OptionParser[AppOptions]("TestST") {
    help("help") text("print help")
    opt[String]("source") text("source path") unbounded() action { (v, c) =>
      c.copy(sources = c.sources :+ Paths.get(v))
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
    opt[String]("zip-charset") text("zip entries charset (if not UTF8)") minOccurs(
      if (Charset.defaultCharset().compareTo(StandardCharsets.UTF_8) == 0) 1 else 0
    ) action { (v, c) =>
      c.copy(zipCharset = Charset.forName(v))
    }

    cmd("test") text("test") action { (_, c) =>
      c.copy(mode = AppMode.test)
    }

    cmd("show-duplicates") text("show duplicates") action { (_, c) =>
      c.copy(mode = AppMode.showDuplicates)
    } children(
      opt[Unit]("by-name") text("by name") action { (_, c) =>
        c.copy(byName = true)
      }
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
      opt[String]("output") text("output path") required() action { (v, c) =>
        c.copy(output = Paths.get(v))
      }
    )
  }

  val options = parser.parse(args, AppOptions()) getOrElse { sys.exit(1) }

  options.mode match {
    case AppMode.test =>
      test()

    case AppMode.showDuplicates =>
      showDuplicates()

    case AppMode.deduplicate =>
      deduplicate()
  }
  println(s"********** Ended: ${new java.util.Date}")

  def test() {
    for (sides <- 1 to 2) {
      for (tracks <- 78 to 82) {
        for (sectors <- 8 to 11) {
          val format = DiskFormat(tracks, sectors, sides)
          println(s"${DiskFormat(format.size)}: ${format.size / 1024}")
        }
      }
    }
  }

  def updateDuplicates(root: Path, map: mutable.Map[String, List[Disk]], key: String, info: DiskInfo) {
    val duplicates = map.getOrElseUpdate(key, Nil)
    map.put(key, Disk(root, info) :: duplicates)
  }

  def atomicName(name: String) =
    /* keep filename */
    name.split("/").toList.reverse.head.
      /* without extension */
      split("""\.""").reverse.tail.reverse.mkString(".")

  def findDuplicates() {
    options.sources foreach { root =>
      println(s"Searching for files in ${root} ...")
      val finder = PathFinder(root) ** """(?i).*\.(?:st|msa|zip)""".r
      val files = finder.get().toList
      println(s"Processing files in ${root} ...")
      files sortBy(_.toString.toLowerCase) map(_.toPath) foreach { path =>
        DiskInfo(path, options.zipCharset, options.zipAllowedExtra).fold ({ ex =>
          ex match {
            case _: NoDiskInZipException =>

            case ex =>
              println(s"Error with file[$path]: ${ex.getMessage()}")
              ex.printStackTrace()
          }
        }, { info =>
          val checksum = info.checksum
          updateDuplicates(root, diskChecksums, info.checksum, info)
          if (options.byName)
            updateDuplicates(root, diskNames, atomicName(info.name.toLowerCase()), info)
        })
      }
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

  def showDuplicates() {
    def showDuplicates(map: mutable.Map[String, List[Disk]]) {
      map.toList sortBy(_._2.head.info.name.toLowerCase()) foreach { tuple =>
        val duplicates = sortDuplicates(tuple._2)
        val preferred = duplicates.preferred
        val unknownFormat = preferred.info.format.isInstanceOf[UnknownDiskFormat]

        if (!duplicates.others.isEmpty || !duplicates.excluded.isEmpty || (unknownFormat && options.warnUnknownFormat)) {
          println(s"Name: ${preferred.info.name}")
          println(s"  Preferred: ${preferred.info}")
          if (options.warnUnknownFormat && unknownFormat)
            println("  WARNING! Unknown disk format")
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
      println("Duplicates by name")
      showDuplicates(diskNames)
      println("")
    }

    println("")
    println("Duplicates by checksum")
    showDuplicates(diskChecksums)
    println("")
  }

  def deduplicate() {
    def move(folder: String, root: Path, path: Path) {
      def findTarget(path: Path, n: Int): Path = {
        val name: String =
          if (n == 0) path.getFileName().toString()
          else {
            val nameSplit = path.getFileName().toString().split("""\.""").toList
            ((nameSplit(0) + s"-$n") :: nameSplit.tail).mkString(".")
          }
        val target = path.resolveSibling(name)
        if (!target.toFile.exists) target
        else {
          println(s"Target already exists: ${target}")
          findTarget(path, n + 1)
        }
      }
      val target = findTarget(options.output.resolve(folder).resolve(root.relativize(path)), 0)
      target.getParent.toFile.mkdirs()
      Files.move(path, target)
    }

    findDuplicates()

    diskChecksums.toList sortBy(_._2.head.info.name.toLowerCase()) foreach { tuple =>
      val duplicates = sortDuplicates(tuple._2)
      val preferred = duplicates.preferred
      val unknownFormat = preferred.info.format.isInstanceOf[UnknownDiskFormat]

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

      if (duplicates.others.isEmpty && duplicates.excluded.isEmpty && unsureDups.isEmpty && (!unknownFormat || !options.warnUnknownFormat))
        println(s"Name: ${preferred.info.name}; Path: ${preferred.info.path}")
      else {
        println(s"Name: ${preferred.info.name}")
        println(s"  Preferred: ${preferred.info}")
      }
      if (options.warnUnknownFormat && preferred.info.format.isInstanceOf[UnknownDiskFormat])
        println("  WARNING! Unknown disk format")
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

}
