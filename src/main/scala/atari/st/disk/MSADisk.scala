package atari.st.disk

import atari.st.disk.exceptions.InvalidFormatException
import java.io.{DataInputStream, FilterInputStream, InputStream}


class MSADisk(input: InputStream) {

  protected val dis = new DataInputStream(input)

  if (dis.readUnsignedShort() != 0x0E0F)
    throw new InvalidFormatException("Invalid magic number")

  val sectorsPerTrack = dis.readUnsignedShort()
  protected val trackSize = sectorsPerTrack * DiskFormat.bytesPerSector
  val sides = dis.readUnsignedShort() + 1
  val trackStart = dis.readUnsignedShort()
  val trackEnd = dis.readUnsignedShort()
  val tracks = trackEnd - trackStart + 1
  val sectors = sectorsPerTrack * tracks * sides

  val filtered = new FilterInputStream(dis) {

    protected var currentTrackSide = 0
    protected var trackData: Option[Array[Byte]] = None
    protected var trackOffset = 0

    protected def nextTrack() =
      if (currentTrackSide == tracks * sides) {
        trackData = None
        trackOffset = 0
        false
      }
      else {
        val compressedTrackSize = dis.readUnsignedShort()
        val array = trackData getOrElse {
          val array = new Array[Byte](trackSize)
          trackData = Some(array)
          array
        }

        if (compressedTrackSize == trackSize) {
          /* not compressed */
          @scala.annotation.tailrec
          def loop(offset: Int): Int =
            dis.read(array, offset, array.length - offset) match {
              case -1 =>
                offset

              case n =>
                val newOffset = offset + n
                if (newOffset >= array.length) newOffset
                else loop(offset + n)
            }

          val actual = loop(0)
          if (actual != trackSize)
            throw new InvalidFormatException(s"Invalid track size: ${actual} expected ${trackSize}")
        }
        else {
          /* compressed */
          @scala.annotation.tailrec
          def loop(offset: Int, remaining: Int): Int = {
            if (remaining <= 0) offset
            else dis.read() match {
              case -1 =>
                offset

              case 0xE5 =>
                val byte = dis.read().byteValue()
                val count = dis.readUnsignedShort()
                for (i <- offset until (offset + count))
                  array(i) = byte
                loop(offset + count, remaining - 4)

              case v =>
                array(offset) = v.byteValue
                loop(offset + 1, remaining - 1)
            }
          }

          val actual = loop(0, compressedTrackSize)
          if (actual != trackSize)
            throw new InvalidFormatException(s"Invalid compressed track size: ${actual} expected ${trackSize}")
        }

        currentTrackSide += 1
        trackOffset = 0
        true
      }

    protected def ensureData() =
      if (trackData.isDefined && (trackOffset < trackSize)) true
      else nextTrack()

    override def read(): Int =
      if (!ensureData()) -1
      else {
        /* Note: make sure to return the byte value in the range 0-255 as needed */
        val r = 0xFF & trackData.get(trackOffset)
        trackOffset += 1
        r
      }

    override def read(buf: Array[Byte], off: Int, len: Int) = {

      @scala.annotation.tailrec
      def loop(off: Int, len: Int): Int =
        if (len <= 0) len
        else if (!ensureData()) len
        else {
          val array = trackData.get
          val need = scala.math.min(len, trackSize - trackOffset)
          Array.copy(array, trackOffset, buf, off, need)
          trackOffset += need
          loop(off + need, len - need)
        }

      if (len <= 0) 0
      else if (!ensureData()) -1
      else len - loop(off, len)
    }

    override def skip(len: Long) = {
      val buffer = new Array[Byte](1024)

      @scala.annotation.tailrec
      def loop(remaining: Long): Long =
        if (remaining <= 0) remaining
        else {
          val len = scala.math.min(remaining, buffer.length).intValue()
          val actual = read(buffer, 0, len)
          if (actual == -1) remaining
          else loop(remaining - actual)
        }

      if (len <= 0) 0
      else len - loop(len)
    }

  }
}
