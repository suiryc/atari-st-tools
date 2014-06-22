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

  /* XXX - use checksum of full disk without disk serial number ?
   *   to find more possible duplicates with different names
   *   to more easily handle by-name duplicates cases ?
   */
  val diskChecksums = mutable.Map[String, Duplicates]()
  val diskNames = mutable.Map[String, List[Disk]]()

  val knownNames = """(?i).*\.(?:st|msa|zip)"""

  def updateDuplicates(map: mutable.Map[String, Duplicates], key: String, disk: Disk) {
    val duplicates =
      map.get(key) map { duplicates =>
        sortDuplicates(duplicates.disks :+ disk)
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
              if (options.verbose > 0)
                println(s"Archive file[$path] does not contain disk images")

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

      val diskName = disk.info.normalizedName
      val duplicatesByName =
        diskNames.get(diskName) map { duplicates =>
          duplicates :+ disk
        } getOrElse(List(disk))
      diskNames.put(diskName, duplicatesByName)
    }
  }

  def sortDuplicates(duplicates: List[Disk], exclude: Boolean = true): Duplicates = {
    val pointsRef = scala.math.max(options.sources.length, 2)

    def pointsNameDepth(info: DiskInfo) =
      pointsRef + 1 - info.name.split("/").length

    def pointsName(info: DiskInfo) = {
      val shortName = PathsEx.atomicName(info.name)

      /* we prefer exact match */
      if (info.path.getFileName().toString.startsWith(s"${shortName}.")) pointsRef
      /* then case insensitive match */
      else if (info.path.getFileName().toString.toLowerCase.startsWith(s"${shortName.toLowerCase}.")) pointsRef / 2
      else 0
    }

    def pointsFileNameLength(info: DiskInfo) =
      info.path.getFileName().toString().length()

    def pointsPath(info: DiskInfo) =
      pointsRef - 1 -
      options.sources.map(info.path.startsWith(_)).zipWithIndex.find(_._1).map(_._2).getOrElse(pointsRef - 1)

    def pointsFormatter(info: DiskInfo) =
      if (info.nameFormatter.isDefined) pointsRef
      else 0

    def pointsNormalizedName(info: DiskInfo) =
      /* we prefer exact match */
      if (info.normalizedName == info.atomicName) pointsRef
      /* then case insensitive match */
      else if (info.atomicName.toLowerCase == info.normalizedName.toLowerCase) pointsRef / 2
      else 0

    def pointsKnownDuplicate(info: DiskInfo) =
      if (Settings.core.duplicatesByName.contains(info.checksum)) pointsRef
      else 0

    def pointsAlternative(info: DiskInfo) =
      if (!folderIsAlternative(info.path.getParent)) 1
      else 0

    def pointsType(info: DiskInfo) =
      info.kind.id

    def pointsFormat(info: DiskInfo) =
      info.format match {
        case format: StandardDiskFormat =>
          val bootSector = info.bootSector
          if ((bootSector.sectorsPerTrack != format.sectorsPerTrack) ||
            (bootSector.tracks != format.tracks) ||
            (bootSector.sides != format.sides))
          {
            pointsRef / 2
          }
          else pointsRef

        case format: UnknownDiskFormat =>
          0
      }

    def points(info: DiskInfo) =
      pointsNameDepth(info) + pointsName(info) + pointsPath(info) +
      pointsFormatter(info) + pointsNormalizedName(info)

    if (duplicates.length > 1) {
      def sort(disks: List[Disk], f: (DiskInfo => Int)*): List[Disk] = {
        def sortDiskList(disks: List[Disk], f: DiskInfo => Int): List[List[Disk]] =
          if (disks.length == 1) List(disks)
          else disks.groupBy(disk => f(disk.info)).toList.sortBy(_._1).reverse.map(_._2)

        @scala.annotation.tailrec
        def sortDiskLists(disks: List[List[Disk]], f: (DiskInfo => Int)*): List[Disk] = {
          f.toList match {
            case fhead :: ftail =>
              val sorted = disks.flatMap(disks => sortDiskList(disks, fhead))
              sortDiskLists(sorted, ftail:_*)

            case Nil =>
              disks.flatten
          }
        }

        if (disks.length < 2) disks
        else sortDiskLists(List(disks), f:_*)
      }

      val (chosens, excluded) =
        if (!exclude) (duplicates, Nil)
        else {
          /* sort by preferred type, drop less rich disk types if any */
          val kind = sort(duplicates, pointsType).head.info.kind
          duplicates.partition(_.info.kind == kind)
        }

      /* Sort and discriminate in the following order:
       *   - known checksum: only matters when comparing different checksums
       *   - disk format (unknown/standard, and compared to boot sector)
       *   - given points: general sorting
       *   - the longest filename: supposed to be more informational
       *   - not in an 'alternative' folder
       *   - richest image type
       */
      val sorted = sort(chosens, pointsKnownDuplicate, pointsFormat, points,
        pointsFileNameLength, pointsAlternative, pointsType)

      Duplicates(sorted.head, sorted.tail, excluded)
    }
    else Duplicates(duplicates.head, Nil, Nil)
  }

  def folderIsAlternative(path: Path): Boolean = Option(path) map { path =>
    val name = path.getFileName.toString
    (name == Settings.core.outputRelativeAlternatives) || (name.startsWith(s"${Settings.core.outputRelativeAlternatives}."))
  } getOrElse(false)

  def folderIsAlternative(disk: Disk): Boolean =
    folderIsAlternative(disk.info.path.getParent)

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
