package atari.st.disk

import atari.st.disk.exceptions.NoDiskInZipException
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.nio.file.Path
import java.security.MessageDigest
import java.util.zip.ZipFile
import scala.language.postfixOps
import scala.util.matching.Regex


object DiskType extends Enumeration {
  val ST = Value
  val MSA = Value
  val Unknown = Value

  val extensions = values.map(_.toString.toLowerCase)
}


case class DiskInfo(path: Path, name: String, kind: DiskType.Value, format: DiskFormat, checksum: String, bootSector: BootSector)

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
          val msa = new MSADisk(input)
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

  def apply(path: Path, zipCharset: Charset = Charset.defaultCharset(), zipAllowedExtraFile: Option[Regex] = None): Either[Exception, DiskInfo] = {
    def build(name: String, kind: DiskType.Value, input: InputStream, size: Int): Either[Exception, DiskInfo] = {
      val r = DiskInfo(path, name, kind, input, size)
      input.close()
      r
    }

    if (extension(path.getFileName().toString) == "zip") {
      import scala.collection.JavaConversions._

      try {
        val zip = new ZipFile(path.toFile, zipCharset)

        val entries = zip.entries().toList map { entry =>
          (entry, imageType(extension(entry.getName())))
        }
        val (knownEntries, unknownEntries) = entries.partition(_._2 != DiskType.Unknown)
        val extraEntries = zipAllowedExtraFile map { regex =>
          unknownEntries.filterNot(entry => regex.pattern.matcher(entry._1.getName()).matches())
        } getOrElse(unknownEntries)
        val r =
          if (entries.length == 0) Left(new Exception("Zip contains nothing"))
          else if (knownEntries.length == 0) Left(new NoDiskInZipException())
          else if (knownEntries.length > 1) Left(new Exception("Zip contains more than one disk"))
          else if (extraEntries.length > 0) Left(new Exception("Zip contains unknown extra entries"))
          else {
            val entry = knownEntries.head._1
            imageType(extension(entry.getName())) match {
              case DiskType.Unknown => Left(new Exception("Unknown disk type"))
              case t => build(entry.getName(), t, zip.getInputStream(entry), entry.getSize().intValue())
            }
          }

        zip.close()
        r
      }
      catch {
        case ex: Throwable =>
          Left(new Exception("Invalid zip file", ex))
      }
    }
    else imageType(extension(path.getFileName.toString)) match {
      case DiskType.Unknown => Left(new Exception("Unknown disk type"))
      case t => build(path.getFileName.toString, t, new BufferedInputStream(new FileInputStream(path.toFile)), path.toFile.length().intValue())
    }
  }

  def extension(name: String) =
    name.toLowerCase.split("""\.""").toList.reverse.head

  def imageType(extension: String): DiskType.Value =
    if (!DiskType.extensions.contains(extension)) DiskType.Unknown
    else DiskType.withName(extension.toUpperCase)

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
