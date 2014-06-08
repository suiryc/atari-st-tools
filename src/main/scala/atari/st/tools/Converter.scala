package atari.st.tools

import atari.st.disk.{Disk, DiskType, MSAOutputDisk, StandardDiskFormat}
import atari.st.disk.exceptions.InvalidFormatException
import atari.st.util.Util
import atari.st.util.zip.Zip
import java.io.{
  BufferedOutputStream,
  ByteArrayOutputStream,
  FileOutputStream,
  InputStream,
  OutputStream
}
import java.nio.charset.Charset
import java.nio.file.Path
import suiryc.scala.io.IOStream


object Converter {

  import Core._

  def convert() {
    val output = options.outputConverted
    val outputType = options.outputType

    findDisks() foreach { disk =>
      checkFormat(disk.info)

      val image = disk.info.getImage()
      val input = image.inputStream

      val targetParent = output.resolve(disk.root.relativize(disk.info.path.getParent))
      val targetName = s"${disk.info.normalizedName}.${outputType.toString.toLowerCase}"
      val targetPath = Util.findTarget(targetParent.resolve(targetName))

      def outputStream(): OutputStream = {
        targetPath.getParent.toFile.mkdirs()
        if (options.dryRun) new ByteArrayOutputStream()
        else new BufferedOutputStream(new FileOutputStream(targetPath.toFile))
      }

      try {
        outputType match {
          case DiskType.ST =>
            val (output, _) = IOStream.transfer(input, outputStream())
            output.flush()
            output.close()

          case DiskType.MSA =>
            disk.info.format match {
              case format: StandardDiskFormat =>
                val msa = new MSAOutputDisk(outputStream(), format)
                val (output, _) = IOStream.transfer(input, msa.filtered)
                output.checkComplete()
                output.flush()
                output.close()

              case _ =>
                throw new InvalidFormatException(s"Cannot convert unknown disk format[${disk.info.format}] to ${outputType}")
            }
        }

        println(s"Converted ${disk.info} to ${targetPath}")
        if (options.zip) {
          println(s"Zip disk image[${targetPath}]")
          if (!options.dryRun)
            Zip.zip(targetPath)
        }
      }
      catch {
        case ex: InvalidFormatException =>
          /* Nothing done */
          println(s"Cannot convert disk[${disk.info.normalizedName}] image[${disk.info.path}]: ${ex.getMessage()}")
          ex.printStackTrace()

        case ex: Throwable =>
          println(s"Cannot convert disk[${disk.info.normalizedName}] image[${disk.info.path}]: ${ex.getMessage()}")
          ex.printStackTrace()
          /* Delete created file */
          if (!options.dryRun)
            targetPath.toFile.delete()
      }
    }
  }

}
