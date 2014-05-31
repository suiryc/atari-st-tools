package atari.st.disk

import atari.st.disk.exceptions.{
  IncompleteImageException,
  InvalidFormatException,
  InvalidImageException
}
import java.io.{ByteArrayInputStream, InputStream}


class DiskImage(val data: Array[Byte], val info: DiskInfo) {

  def inputStream =
    DiskImage.dataToStream(data)

}

object DiskImage {

  def loadImage(input: InputStream, size: Int) = {
    val data = new Array[Byte](size)

    @scala.annotation.tailrec
    def loop(offset: Int): Int =
      if (offset >= size) offset
      else {
        val read = input.read(data, offset, size - offset)
        if (read == -1) offset
        else loop(offset + read)
      }

    val actual = loop(0)
    if (actual != size)
      throw new IncompleteImageException(s"Loaded ${actual}, expected ${size}")
    data
  }

  def readDiskFormat(data: Array[Byte]): DiskFormat = {
    /* Some details: http://info-coach.fr/atari/software/FD-Soft.php */

    /* Boot sector is the first */
    if (data.length < 512)
      throw new InvalidFormatException("Image has no boot sector")

    val input = dataToStream(data)
    input.skip(11)
    val bytesPerSector = ProtocolStream.readInteger(input, BitSize.Short).intValue()
    input.skip(6)
    val sectors = ProtocolStream.readInteger(input, BitSize.Short).intValue()
    input.skip(3)
    val sectorsPerTrack = ProtocolStream.readInteger(input, BitSize.Short).intValue()
    /* Actually 'heads', but 'sides' is more common for floppy disks */
    val sides = ProtocolStream.readInteger(input, BitSize.Short).intValue()
    input.close()

    if (sectors * DiskFormat.bytesPerSector != data.length) {
      /* Either boot sector has wrong info, or image is not complete.
       * In both cases, try to guess.
       */
      DiskFormat(data.length)
    }
    else {
      val tracks = sectors / (sectorsPerTrack * sides)
      DiskFormat(sectors, tracks, sectorsPerTrack, sides)
    }
  }

  def dataToStream(data: Array[Byte]) =
    new ByteArrayInputStream(data)

}
