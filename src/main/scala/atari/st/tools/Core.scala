package atari.st.tools

import atari.st.DiskTools
import atari.st.disk.{
  Disk,
  DiskInfo,
  Duplicates,
  StandardDiskFormat,
  UnknownDiskFormat
}
import atari.st.disk.exceptions.NoDiskInZipException
import atari.st.settings.Settings
import java.nio.file.{Files, Path}
import scala.collection.mutable
import suiryc.scala.io.{PathFinder, PathsEx}
import suiryc.scala.io.NameFilter._


object Core {

  val options = DiskTools.options
  implicit val charset = options.zipCharset

  val diskChecksums = mutable.Map[String, Duplicates]()
  val diskNames = mutable.Map[String, Duplicates]()

  val knownNames = """(?i).*\.(?:st|msa|zip)"""

  def updateDuplicates(map: mutable.Map[String, Duplicates], key: String, disk: Disk) {
    val duplicates =
      map.get(key) map { duplicates =>
        sortDuplicates(disk :: duplicates.disks)
      } getOrElse(Duplicates(disk, Nil, Nil))
    map.put(key, duplicates)
  }

  def findFiles[T](process: (Path, Path, List[Path]) => T): List[T] =
    options.sources map { source =>
      val (root, files) =
        if (Files.isDirectory(source)) {
          println(s"Searching for files in ${source} ...")
          val finder = PathFinder(source) ** knownNames.r
          val files = finder.get().toList map(_.toPath) sortBy(_.toString.toLowerCase)
          (source, files)
        }
        else if (Files.exists(source)) {
          val files =
            if (!source.getFileName.toString.matches(knownNames)) Nil
            else List(source)
          (source.getParent, files)
        }
        else {
          println(s"Path ${source} does not exist.")
          (source, Nil)
        }
      if (files.isEmpty && Files.exists(source))
        println(s"Path ${source} does not contain known files")
      process(source, root, files)
    }

  def findDisks(): List[Disk] = {
    def findDisks(source: Path, root: Path, files: List[Path]): List[Disk] = {
      if (!files.isEmpty) {
        if (Files.isDirectory(source)) {
          println(s"Loading files info in ${source} ...")
        }
        else if (Files.exists(source)) {
          println(s"Loading file ${source} info ...")
        }
      }
      files sortBy(_.toString.toLowerCase) map { path =>
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

    findFiles(findDisks).flatten
  }

  def findDuplicates() {
    val disks = findDisks()
    println("Searching duplicates ...")
    disks foreach { disk =>
      updateDuplicates(diskChecksums, disk.info.checksum, disk)
      updateDuplicates(diskNames, disk.info.normalizedName, disk)
    }
  }

  def sortDuplicates(duplicates: List[Disk], exclude: Boolean = true): Duplicates = {
    val pointsRef = scala.math.max(options.sources.length, 2)

    def pointsNameDepth(name: String) =
      pointsRef + 1 - name.split("/").length

    def pointsName(name: String, path: Path) = {
      val shortName = PathsEx.atomicName(name)

      /* we prefer exact match */
      if (path.getFileName().toString.startsWith(s"${shortName}.")) pointsRef
      /* then case insensitive match */
      else if (path.getFileName().toString.toLowerCase.startsWith(s"${shortName.toLowerCase}.")) pointsRef / 2
      else 0
    }

    def pointsFileNameLength(path: Path) =
      path.getFileName().toString().length()

    def pointsPath(path: Path) =
      pointsRef - 1 -
      options.sources.map(path.startsWith(_)).zipWithIndex.find(_._1).map(_._2).getOrElse(pointsRef - 1)

    def pointsFormatter(info: DiskInfo) =
      if (info.nameFormatter.isDefined) pointsRef
      else 0

    def pointsNormalizedName(info: DiskInfo) =
      /* we prefer exact match */
      if (info.normalizedName == info.atomicName) pointsRef
      /* then case insensitive match */
      else if (info.atomicName.toLowerCase == info.normalizedName.toLowerCase) pointsRef / 2
      else 0

    def points(info: DiskInfo) =
      pointsNameDepth(info.name) + pointsName(info.name, info.path) +
      pointsPath(info.path) + pointsFormatter(info) + pointsNormalizedName(info)

    if (duplicates.length > 1) {
      val (chosens, excluded) =
        if (!exclude) (duplicates, Nil)
        else {
          /* sort by preferred format */
          val sorted = duplicates.sortBy(_.info.kind.id).reverse
          val sample1 = sorted.head.info
          /* drop less rich disk types if any */
          sorted.partition(_.info.kind == sample1.kind)
        }
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
          println(s"WARNING! Disk[${info.normalizedName}] image[${info.path}] has unknown format: ${format}")

      case format: StandardDiskFormat =>
        val bootSector = info.bootSector
        if (options.checkBootSector || force) {
          if ((bootSector.sectorsPerTrack != format.sectorsPerTrack) ||
            (bootSector.tracks != format.tracks) ||
            (bootSector.sides != format.sides))
          {
            println(s"WARNING! Disk[${info.normalizedName}] image[${info.path}] ${bootSector} does not match ${format}")
          }
        }
    }
  }

}
