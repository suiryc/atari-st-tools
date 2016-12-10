package atari.st.disk

import java.io.ByteArrayInputStream
import java.security.MessageDigest
import suiryc.scala.io.IOStream


case class BootSector(
  data: Array[Byte],
  bytesPerSector: Int,
  sectors: Int,
  sectorsPerTrack: Int,
  sides: Int
) {

  override def toString: String =
    s"BootSector($bytesPerSector,$sectors,$sectorsPerTrack,$sides,$checksum)"

  val tracks: Int =
    if ((sectorsPerTrack <= 0) || (sides <= 0)) -1
    else sectors / (sectorsPerTrack * sides)

  val checksum: String = {
    val input = new ByteArrayInputStream(data)
    val msgDigest = MessageDigest.getInstance("MD5")

    def updateChecksum(b: Array[Byte], off: Int, len: Int) {
      msgDigest.update(b, 0, len)
    }

    /* Serial number is 3 bytes at offset 8 */
    IOStream.process(input, updateChecksum, len = Some(8))
    IOStream.skipFully(input, 3)
    /* 'Checksum' are last 2 bytes of sector */
    IOStream.process(input, updateChecksum, len = Some(DiskFormat.bytesPerSector - 8 - 3 - 2))

    msgDigest.digest().toList.map { byte =>
      f"$byte%02X"
    }.mkString
  }

}
