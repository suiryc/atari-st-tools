package atari.st.disk

import atari.st.disk.exceptions.NoDiskInZipException
import atari.st.settings.Settings
import atari.st.util.zip.Zip
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.nio.file.Path
import java.security.MessageDigest
import java.util.zip.{ZipEntry, ZipException, ZipFile}
import scala.util.matching.Regex
import suiryc.scala.RichEnumeration
import suiryc.scala.io.{FileTimes, IOStream, PathsEx}


object DiskType extends Enumeration {
  val ST = Value
  val MSA = Value
  val Unknown = Value
}


case class DiskInfo(
  path: Path,
  name: String,
  kind: DiskType.Value,
  times: FileTimes,
  format: DiskFormat,
  /** Disk checksum */
  checksum: String,
  /** Disk without boot sector checksum */
  checksum2: String,
  bootSector: BootSector
) {

  override def toString: String =
    s"DiskInfo($path,$name,$checksum,$checksum2,$format,$bootSector,${times.lastModified})"

  val atomicName: String = PathsEx.atomicName(name)

  val nameFormatter: Option[RegexDiskNameFormatter] =
    DiskInfo.nameFormatter(atomicName)

  val normalizedName: String = {
    val formatter =
      nameFormatter.getOrElse(DiskNameFormatter.lowerCase)

    formatter.normalize(atomicName)
  }

  def getImage()(implicit zipCharset: Charset): DiskImage = {
    def build(input: InputStream): DiskImage = {
      val r = kind match {
        case DiskType.ST =>
          val data = DiskImage.loadImage(input, format.size)
          new DiskImage(data, this)

        case DiskType.MSA =>
          val msa = new MSAInputDisk(input)
          val data = DiskImage.loadImage(msa.filtered, format.size)
          new DiskImage(data, this)
      }
      input.close()
      r
    }

    if (PathsEx.extension(path).toLowerCase == "zip") {
      import scala.collection.JavaConverters._

      val zip = new ZipFile(path.toFile, zipCharset)
      val entry = zip.entries().asScala.toList.find(_.getName == name).get
      zip.close()
      Zip.unzip[Option[DiskImage]](path, { (_entry, input) =>
        /* Continue until we find the entry */
        val found = _entry.getName == entry.getName
        val value =
          if (found) Some(build(input))
          else None
        (value, !found)
      }).collectFirst {
        case Some(disk) => disk
      }.getOrElse {
        val ex = new ZipException("ZipFile/ZipInputStream mismatch")
        ex.initCause(new Exception(s"Missing entry: ${entry.getName}"))
        throw ex
      }
    }
    else {
      build(new BufferedInputStream(new FileInputStream(path.toFile)))
    }
  }

}

object DiskInfo {

  def apply(path: Path, name: String, kind: DiskType.Value, times: FileTimes, input: InputStream, size: Int): Either[Exception, DiskInfo] =
    kind match {
      case DiskType.ST =>
        try {
          val data = DiskImage.loadImage(input, size)
          val bootSector = DiskImage.readBootSector(data)
          val format = DiskFormat(bootSector, size)
          val imageStream = DiskImage.dataToStream(data)
          val (checksum, checksum2) = computeChecksum(imageStream)
          Right(DiskInfo(path, name, kind, times, format, checksum, checksum2, bootSector))
        }
        catch {
          case ex: Exception =>
            Left(ex)
        }

      case DiskType.MSA =>
        try {
          val msa = new MSAInputDisk(input)
          val data = DiskImage.loadImage(msa.filtered, msa.size)
          val bootSector = DiskImage.readBootSector(data)
          val format = DiskFormat(msa.sectors, msa.tracks, msa.sectorsPerTrack, msa.sides)
          val imageStream = DiskImage.dataToStream(data)
          val (checksum, checksum2) = computeChecksum(imageStream)
          Right(DiskInfo(path, name, kind, times, format, checksum, checksum2, bootSector))
        }
        catch {
          case ex: Exception =>
            Left(ex)
        }
    }

  def apply(path: Path, zipAllowDiskName: Boolean, zipAllowExtra: List[Regex])(implicit zipCharset: Charset): Either[Exception, DiskInfo] = {
    def build(name: String, kind: DiskType.Value, times: FileTimes, input: InputStream, size: Int): Either[Exception, DiskInfo] = {
      val r = DiskInfo(path, name, kind, times, input, size)
      input.close()
      r
    }

    if (PathsEx.extension(path).toLowerCase == "zip") {
      try {
        val zip = new ZipFile(path.toFile, zipCharset)
        val entries = zipEntries(zip, zipAllowDiskName, zipAllowExtra)
        zip.close()

        if (entries.count == 0) Left(new Exception("Zip contains nothing"))
        else if (entries.disks.isEmpty) Left(new NoDiskInZipException())
        else if (entries.disks.length > 1) Left(new Exception("Zip contains more than one disk"))
        else if (entries.extraUnknown.nonEmpty) Left(new Exception("Zip contains unknown extra entries"))
        else {
          val entry = entries.disks.head.entry
          imageType(PathsEx.extension(entry.getName)) match {
            case DiskType.Unknown =>
              Left(new Exception("Unknown disk type"))

            case t =>
              Zip.unzip[Option[Either[Exception, DiskInfo]]](path, { (_entry, input) =>
                /* Continue until we find the entry */
                val found = _entry.getName == entry.getName
                val value =
                  if (found) Some(build(entry.getName, t, FileTimes(entry), input, entry.getSize.intValue()))
                  else None
                (value, !found)
              }).collectFirst {
                case Some(r) => r
              }.getOrElse {
                val ex = new ZipException("ZipFile/ZipInputStream mismatch")
                ex.initCause(new Exception(s"Missing entry: ${entry.getName}"))
                Left(ex)
              }
          }
        }
      }
      catch {
        case ex: Exception =>
          Left(ex)

        case ex: Throwable =>
          Left(new Exception("Invalid zip file", ex))
      }
    }
    else imageType(PathsEx.extension(path)) match {
      case DiskType.Unknown =>
        Left(new Exception("Unknown disk type"))

      case t =>
        val input = new BufferedInputStream(new FileInputStream(path.toFile))
        build(path.getFileName.toString, t, FileTimes(path), input, path.toFile.length().intValue())
    }
  }

  case class ZipEntryType(entry: ZipEntry, kind: DiskType.Value)
  case class ZipEntries(count: Int, disks: List[ZipEntryType], extraAllowed: List[ZipEntry], extraUnknown: List[ZipEntry])

  def zipEntries(zip: ZipFile, zipAllowDiskName: Boolean, zipAllowExtra: List[Regex]): ZipEntries = {
    import scala.collection.JavaConverters._

    val tuples = zip.entries().asScala.toList filterNot(_.isDirectory) sortBy(_.getName.toLowerCase) map { entry =>
      (entry, imageType(PathsEx.extension(entry.getName)))
    }
    val (knownTuples, otherTuples) = tuples.partition(_._2 != DiskType.Unknown)
    val disks = knownTuples map { tuple =>
      ZipEntryType(tuple._1, tuple._2)
    }
    val diskEntry = knownTuples.headOption.map(_._1)
    val (extraAllowed, extraUnknown) = otherTuples map(_._1) partition { entry =>
      val entryName = entry.getName
      (zipAllowExtra exists { regex =>
        regex.pattern.matcher(PathsEx.filename(entryName)).matches()
      }) || (zipAllowDiskName && diskEntry.exists { diskEntry =>
        PathsEx.atomicName(entryName).toLowerCase == PathsEx.atomicName(diskEntry.getName.toLowerCase)
      })
    }

    ZipEntries(tuples.length, disks, extraAllowed, extraUnknown)
  }

  def nameFormatter(name: String): Option[RegexDiskNameFormatter] =
    Settings.core.diskNameFormatters find { formatter =>
      formatter.matches(name)
    }

  def imageType(extension: String): DiskType.Value =
    try {
      DiskType.byName(extension)
    }
    catch {
      case _: Throwable => DiskType.Unknown
    }

  /* Computes checksums (upper-case) of given input. */
  def computeChecksum(stream: InputStream): (String, String) = {
    val msgDigest = MessageDigest.getInstance("MD5")
    val msgDigest2 = MessageDigest.getInstance("MD5")

    def updateChecksums(bootsector: Boolean)(b: Array[Byte], off: Int, len: Int) {
      msgDigest.update(b, 0, len)
      if (!bootsector)
        msgDigest2.update(b, 0, len)
    }

    /* Separately read the boot sector */
    IOStream.process(stream, updateChecksums(bootsector = true), len = Some(DiskFormat.bytesPerSector))
    /* Then read the rest of the disk */
    IOStream.process(stream, updateChecksums(bootsector = false))

    val checksum =
      msgDigest.digest().toList.map { byte =>
        f"$byte%02X"
      }.mkString
    val checksum2 =
      msgDigest2.digest().toList.map { byte =>
        f"$byte%02X"
      }.mkString

    (checksum, checksum2)
  }

}
