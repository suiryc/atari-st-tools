package atari.st.tools

import atari.st.DiskTools
import atari.st.disk.{Disk, DiskInfo, StandardDiskFormat, UnknownDiskFormat}
import atari.st.disk.exceptions.NoDiskInZipException
import java.nio.file.{Files, Path}
import scala.collection.mutable
import suiryc.scala.io.PathFinder
import suiryc.scala.io.NameFilter._


object Core {

  val options = DiskTools.options
  implicit val charset = options.zipCharset

  val diskChecksums = mutable.Map[String, List[Disk]]()
  val diskNames = mutable.Map[String, List[Disk]]()

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

}
