package atari.st.disk

import atari.st.disk.exceptions.InvalidFormatException
import java.io.{
  BufferedOutputStream,
  DataInputStream,
  DataOutputStream,
  EOFException,
  FilterInputStream,
  FilterOutputStream,
  InputStream,
  OutputStream
}


object MSADisk {

  val magicHeader = 0x0E0F

}

class MSAInputDisk(input: InputStream) {

  import MSADisk._

  protected val dis = new DataInputStream(input)

  if (dis.readUnsignedShort() != magicHeader)
    throw new InvalidFormatException("Invalid magic number")

  val sectorsPerTrack: Int = dis.readUnsignedShort()
  protected val trackSize = sectorsPerTrack * DiskFormat.bytesPerSector
  val sides: Int = dis.readUnsignedShort() + 1
  val trackStart: Int = dis.readUnsignedShort()
  val trackEnd: Int = dis.readUnsignedShort()
  val tracks: Int = trackEnd - trackStart + 1
  val sectors: Int = sectorsPerTrack * tracks * sides
  val size: Int = sectors * DiskFormat.bytesPerSector

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
            throw new InvalidFormatException(s"Invalid track size: $actual expected $trackSize")
        }
        else {
          /* compressed */
          @inline def uncompressedByte(offset: Int, b: Byte) {
            if (offset >= trackSize)
              throw new InvalidFormatException(s"Invalid compressed track size: can uncompress more than expected $trackSize")
            array(offset) = b
          }

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
                  uncompressedByte(i, byte)
                loop(offset + count, remaining - 4)

              case v =>
                uncompressedByte(offset, v.byteValue)
                loop(offset + 1, remaining - 1)
            }
          }

          val actual = loop(0, compressedTrackSize)
          if (actual != trackSize)
            throw new InvalidFormatException(s"Invalid compressed track size: uncompressed $actual expected $trackSize")
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

    override def read(buf: Array[Byte], off: Int, len: Int): Int = {

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

    override def skip(len: Long): Long = {
      val buffer = new Array[Byte](1024)

      @scala.annotation.tailrec
      def loop(remaining: Long): Long =
        if (remaining <= 0) remaining
        else {
          val len = scala.math.min(remaining, buffer.length.toLong).intValue()
          val actual = read(buffer, 0, len)
          if (actual == -1) remaining
          else loop(remaining - actual)
        }

      if (len <= 0) 0
      else len - loop(len)
    }

  }
}

class MSAOutputDisk(output: OutputStream, format: StandardDiskFormat) {

  import MSADisk._

  protected val dos = new DataOutputStream(output)

  dos.writeShort(magicHeader)
  dos.writeShort(format.sectorsPerTrack)
  protected val trackSize = format.sectorsPerTrack * DiskFormat.bytesPerSector
  dos.writeShort(format.sides - 1)
  dos.writeShort(0)
  dos.writeShort(format.tracks - 1)

  val filtered = new MASOutputStream(new BufferedOutputStream(dos))

  class MASOutputStream(out: OutputStream)
  extends FilterOutputStream(out)
  {

    protected var currentTrackSide = 0
    protected var trackData: Option[Array[Byte]] = None
    protected var trackOffset = 0

    protected def nextTrack(track: Array[Byte]) {
      val compressed = new Array[Byte](trackSize)

      /* Encoding takes 4 bytes:
       *  - 0xE5 marker
       *  - 1 byte: value to repeat
       *  - 2 bytes: repeat count
       * Even though encoding a byte repeated 4 times saves no space,
       * implementations still do it.
       * Thus the rules are:
       *  - encode a byte if repeated at least 4 times
       *  - always encode 0xE5 (the marker value)
       *  - only keep the compressed track if its size is less than the
       *    uncompressed track
       */
      @scala.annotation.tailrec
      def compress(compressedOffset: Int, trackOffset: Int): Int =
        /* Note: give up as soon as we reach uncompressed track size */
        if ((trackOffset == trackSize) || (compressedOffset == trackSize)) compressedOffset
        else {
          val b = track(trackOffset)
          @scala.annotation.tailrec
          def loop(trackOffset: Int, repeat: Int): Int =
            if (trackOffset == trackSize) repeat
            else if (track(trackOffset) == b) loop(trackOffset + 1, repeat + 1)
            else repeat
          val repeat = loop(trackOffset + 1, 1)

          /* Note: *always* encode the marker */
          if ((repeat > 4) || (b == 0xE5.byteValue())) {
            if (compressedOffset + 4 >= trackSize) trackSize
            else {
              compressed(compressedOffset) = 0xE5.byteValue()
              compressed(compressedOffset + 1) = b
              compressed(compressedOffset + 2) = ((repeat >>> 8) & 0xFF).byteValue()
              compressed(compressedOffset + 3) = ((repeat >>> 0) & 0xFF).byteValue()
              compress(compressedOffset + 4, trackOffset + repeat)
            }
          }
          else {
            compressed(compressedOffset) = b
            compress(compressedOffset + 1, trackOffset + 1)
          }
        }

      val compressedOffset = compress(0, 0)
      if (compressedOffset < trackSize) {
        /* compressed */
        dos.writeShort(compressedOffset)
        dos.write(compressed, 0, compressedOffset)
      }
      else {
        /* not compressed */
        dos.writeShort(track.length)
        dos.write(track)
      }

      currentTrackSide += 1
      trackOffset = 0
    }

    override def write(b: Int) {
      if (currentTrackSide == format.tracks * format.sides)
        throw new EOFException("Cannot write beyond disk format")

      val array = trackData getOrElse {
        val array = new Array[Byte](trackSize)
        trackData = Some(array)
        array
      }

      array(trackOffset) = b.byteValue()
      trackOffset += 1
      if (trackOffset == trackSize)
        nextTrack(array)
    }

    def checkComplete() {
      if (currentTrackSide != format.tracks * format.sides)
        throw new EOFException("Did not write full disk")
    }

  }

}
