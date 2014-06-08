package atari.st.disk

import atari.st.disk.exceptions.NoDiskInZipException
import atari.st.settings.Settings
import atari.st.util.Util
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.nio.file.Path
import java.security.MessageDigest
import java.util.zip.{ZipEntry, ZipException, ZipFile}
import scala.language.postfixOps
import scala.util.matching.Regex
import suiryc.scala.misc.EnumerationEx


object DiskType extends EnumerationEx {
  val ST = Value
  val MSA = Value
  val Unknown = Value
}


case class DiskInfo(
  path: Path,
  name: String,
  kind: DiskType.Value,
  format: DiskFormat,
  checksum: String,
  bootSector: BootSector
) {

  import DiskInfo._

  val atomicName = DiskInfo.atomicName(name)

  val nameFormatter =
    DiskInfo.nameFormatter(atomicName)

  val normalizedName = {
    val formatter =
      nameFormatter getOrElse(DiskNameFormatter.lowerCase)

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

    if (extension(path.getFileName().toString).toLowerCase == "zip") {
      import scala.collection.JavaConversions._

      val zip = new ZipFile(path.toFile, zipCharset)
      val entry = zip.entries().toList.find(_.getName() == name).get
      zip.close()
      Util.unzip(path, { (_entry, input) =>
        /* Continue until we find the entry */
        val found = (_entry.getName() == entry.getName())
        val value =
          if (found) Some(build(input))
          else None
        (value, !found)
      }) collectFirst {
        case Some(disk) => disk
      } getOrElse {
        val ex = new ZipException("ZipFile/ZipInputStream mismatch")
        ex.initCause(new Exception(s"Missing entry: ${entry.getName()}"))
        throw ex
      }
    }
    else {
      build(new BufferedInputStream(new FileInputStream(path.toFile)))
    }
  }

}

object DiskInfo {

  def apply(path: Path, name: String, kind: DiskType.Value, input: InputStream, size: Int): Either[Exception, DiskInfo] =
    kind match {
      case DiskType.ST =>
        try {
          val data = DiskImage.loadImage(input, size)
          val bootSector = DiskImage.readBootSector(data)
          val format = DiskFormat(bootSector, size)
          val imageStream = DiskImage.dataToStream(data)
          Right(DiskInfo(path, name, kind, format, computeChecksum(imageStream), bootSector))
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
          val imageStream = DiskImage.dataToStream(data)
          Right(DiskInfo(path, name, kind, DiskFormat(msa.sectors, msa.tracks, msa.sectorsPerTrack, msa.sides), computeChecksum(imageStream), bootSector))
        }
        catch {
          case ex: Exception =>
            Left(ex)
        }
    }

  def apply(path: Path, zipAllowDiskName: Boolean, zipAllowExtra: List[Regex])(implicit zipCharset: Charset): Either[Exception, DiskInfo] = {
    def build(name: String, kind: DiskType.Value, input: InputStream, size: Int): Either[Exception, DiskInfo] = {
      val r = DiskInfo(path, name, kind, input, size)
      input.close()
      r
    }

    if (extension(path.getFileName().toString).toLowerCase == "zip") {
      import scala.collection.JavaConversions._

      try {
        val zip = new ZipFile(path.toFile, zipCharset)
        val entries = zipEntries(zip, zipAllowDiskName, zipAllowExtra)
        zip.close()

        if (entries.count == 0) Left(new Exception("Zip contains nothing"))
        else if (entries.disks.length == 0) Left(new NoDiskInZipException())
        else if (entries.disks.length > 1) Left(new Exception("Zip contains more than one disk"))
        else if (entries.extraUnknown.length > 0) Left(new Exception("Zip contains unknown extra entries"))
        else {
          val entry = entries.disks.head.entry
          imageType(extension(entry.getName())) match {
            case DiskType.Unknown =>
              Left(new Exception("Unknown disk type"))

            case t =>
              Util.unzip(path, { (_entry, input) =>
                /* Continue until we find the entry */
                val found = (_entry.getName() == entry.getName())
                val value =
                  if (found) Some(build(entry.getName(), t, input, entry.getSize().intValue()))
                  else None
                (value, !found)
              }) collectFirst {
                case Some(r) => r
              } getOrElse {
                val ex = new ZipException("ZipFile/ZipInputStream mismatch")
                ex.initCause(new Exception(s"Missing entry: ${entry.getName()}"))
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
    else imageType(extension(path.getFileName.toString)) match {
      case DiskType.Unknown => Left(new Exception("Unknown disk type"))
      case t => build(path.getFileName.toString, t, new BufferedInputStream(new FileInputStream(path.toFile)), path.toFile.length().intValue())
    }
  }

  case class ZipEntryType(entry: ZipEntry, kind: DiskType.Value)
  case class ZipEntries(count: Int, disks: List[ZipEntryType], extraAllowed: List[ZipEntry], extraUnknown: List[ZipEntry])

  def zipEntries(zip: ZipFile, zipAllowDiskName: Boolean, zipAllowExtra: List[Regex]): ZipEntries = {
    import scala.collection.JavaConversions._

    val tuples = zip.entries().toList filterNot(_.isDirectory) sortBy(_.getName.toLowerCase) map { entry =>
      (entry, imageType(extension(entry.getName())))
    }
    val (knownTuples, otherTuples) = tuples.partition(_._2 != DiskType.Unknown)
    val disks = knownTuples map { tuple =>
      ZipEntryType(tuple._1, tuple._2)
    }
    val diskEntry = knownTuples.headOption.map(_._1)
    val (extraAllowed, extraUnknown) = otherTuples map(_._1) partition { entry =>
      val entryName = entry.getName()
      (zipAllowExtra exists { regex =>
        regex.pattern.matcher(filename(entryName)).matches()
      }) || (zipAllowDiskName && diskEntry.exists { diskEntry =>
        atomicName(entryName).toLowerCase == atomicName(diskEntry.getName().toLowerCase)
      })
    }

    ZipEntries(tuples.length, disks, extraAllowed, extraUnknown)
  }

  def filename(name: String) =
    name.split("/").toList.reverse.head

  def atomicName(name: String) =
    /* keep filename */
    filename(name).
      /* without extension */
      split("""\.""").reverse.tail.reverse.mkString(".")

  def extension(name: String) =
    name.split("""\.""").toList.reverse.head

  def nameFormatter(name: String) =
    Settings.core.diskNameFormatters find { formatter =>
      formatter.matches(name)
    }

  def imageType(extension: String): DiskType.Value =
    try {
      DiskType(extension)
    }
    catch {
      case _: Throwable => DiskType.Unknown
    }

  def computeChecksum(stream: InputStream): String = {
    val buffer = new Array[Byte](1024 * 16)
    val msgDigest = MessageDigest.getInstance("MD5")

    Stream.continually(stream.read(buffer)).takeWhile(_ != -1) foreach { count =>
      msgDigest.update(buffer, 0, count)
    }
    msgDigest.digest().toList map { byte =>
      f"$byte%02X"
    } mkString
  }

}
