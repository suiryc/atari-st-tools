package atari.st.disk

import atari.st.disk.exceptions.NoDiskInZipException
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.nio.file.Path
import java.security.MessageDigest
import java.util.zip.ZipFile
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

  val atomicName = DiskInfo.atomicName(name).toLowerCase

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
      val r = build(zip.getInputStream(entry))
      zip.close()
      r
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

        val entries = zip.entries().toList map { entry =>
          (entry, imageType(extension(entry.getName())))
        }
        val (knownTuples, unknownTuples) = entries.partition(_._2 != DiskType.Unknown)
        val diskEntry = knownTuples.headOption.map(_._1)
        val extraTuples = unknownTuples filterNot { tuple =>
          val entryName = tuple._1.getName()
          (zipAllowExtra exists { regex =>
            regex.pattern.matcher(filename(entryName)).matches()
          }) || (zipAllowDiskName && diskEntry.exists { diskEntry =>
            DiskInfo.atomicName(entryName).toLowerCase == DiskInfo.atomicName(diskEntry.getName().toLowerCase)
          })
        }
        val r =
          if (entries.length == 0) Left(new Exception("Zip contains nothing"))
          else if (knownTuples.length == 0) Left(new NoDiskInZipException())
          else if (knownTuples.length > 1) Left(new Exception("Zip contains more than one disk"))
          else if (extraTuples.length > 0) Left(new Exception("Zip contains unknown extra entries"))
          else {
            val entry = diskEntry.get
            imageType(extension(entry.getName())) match {
              case DiskType.Unknown => Left(new Exception("Unknown disk type"))
              case t => build(entry.getName(), t, zip.getInputStream(entry), entry.getSize().intValue())
            }
          }

        zip.close()
        r
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

  def filename(name: String) =
    name.split("/").toList.reverse.head

  def atomicName(name: String) =
    /* keep filename */
    filename(name).
      /* without extension */
      split("""\.""").reverse.tail.reverse.mkString(".")

  def extension(name: String) =
    name.split("""\.""").toList.reverse.head

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
