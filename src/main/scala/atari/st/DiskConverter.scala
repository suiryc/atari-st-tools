package atari.st

import atari.st.disk.{Disk, DiskType, MSAOutputDisk, StandardDiskFormat}
import atari.st.util.Util
import java.io.{
  FileOutputStream,
  InputStream,
  OutputStream
}
import java.nio.charset.Charset
import java.nio.file.Path


object DiskConverter {

  def convert(disk: Disk, output: Path, outputType: DiskType.Value)(implicit zipCharset: Charset) {
    val image = disk.info.getImage()
    val input = image.inputStream

    val targetParent = output.resolve(disk.root.relativize(disk.info.path.getParent))
    val targetName = (outputType.toString.toLowerCase :: disk.info.name.toLowerCase.split("""\.""").toList.reverse.tail).reverse.mkString(".")
    val target = Util.findTarget(targetParent.resolve(targetName))

    def outputStream(): OutputStream = {
      target.getParent.toFile.mkdirs()
      new FileOutputStream(target.toFile)
    }

    def transfer(output: OutputStream): OutputStream = {
      val buffer = new Array[Byte](16 * 1024)
      Stream.continually(input.read(buffer)) takeWhile(_ != -1) foreach { count =>
        output.write(buffer, 0 , count)
      }
      output
    }

    outputType match {
      case DiskType.ST =>
        val output = transfer(outputStream())
        output.flush()
        output.close()

      case DiskType.MSA =>
        disk.info.format match {
          case format: StandardDiskFormat =>
            val msa = new MSAOutputDisk(outputStream(), format)
            val output = transfer(msa.filtered)
            msa.filtered.checkComplete()
            output.flush()
            output.close()

          case _ =>
            throw new Exception("Cannot convert unknown disk format[${format}] to MSA")
        }
    }

    println(s"Converted ${disk.info} to $target")
  }

}
