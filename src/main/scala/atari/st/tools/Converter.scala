package atari.st.tools

import atari.st.disk.{Disk, DiskType, MSAOutputDisk, StandardDiskFormat}
import atari.st.util.Util
import java.io.{
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
      val target = Util.findTarget(targetParent.resolve(targetName))

      def outputStream(): OutputStream = {
        target.getParent.toFile.mkdirs()
        new FileOutputStream(target.toFile)
      }

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
              throw new Exception("Cannot convert unknown disk format[${format}] to MSA")
          }
      }

      println(s"Converted ${disk.info} to $target")
    }
  }

}
