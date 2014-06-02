package atari.st.disk

import atari.st.disk.exceptions.{
  InvalidFormatException,
  InvalidImageException
}
import java.io.{ByteArrayInputStream, InputStream}


case class BootSector(
  bytesPerSector: Int,
  sectors: Int,
  sectorsPerTrack: Int,
  sides: Int
) {

  val tracks =
    if ((sectorsPerTrack <= 0) || (sides <= 0)) -1
    else sectors / (sectorsPerTrack * sides)

}

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
      throw new InvalidImageException(s"Loaded ${actual}, expected ${size}")
    if (input.read() != -1)
      throw new InvalidImageException(s"Could load more than expected ${size}")
    data
  }

  def readBootSector(data: Array[Byte]): BootSector = {
    /* Some details: http://info-coach.fr/atari/software/FD-Soft.php */

    /* Boot sector is the first */
    if (data.length < DiskFormat.bytesPerSector)
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

    BootSector(bytesPerSector, sectors, sectorsPerTrack, sides)
  }

  def dataToStream(data: Array[Byte]) =
    new ByteArrayInputStream(data)

}
