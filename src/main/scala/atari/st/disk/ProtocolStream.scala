package atari.st.disk

import java.io.{EOFException, InputStream}

/**
 * Protocol data bit size.
 */
object BitSize {
  /** Byte is 8-bits. */
  val Byte = 8
  /** Short is 16-bits. */
  val Short = 16
  /** Int is 32-bits. */
  val Int = 32
}

/**
 * Protocol stream helper methods.
 *
 * @note Integers are written in little-endian order.
 * @note Integers (byte, short and int) are unsigned values. Since ''Scala'' (as
 *   in ''Java'') only manipulates signed values, it is often necessary to use a
 *   larger integer type to hold and correctly use them.
 */
object ProtocolStream {

  /* Constants about Long values.
   * Mainly used to prevent warnings about magic numbers. */
  final private val LONG_BITSIZE = 64
  final private val LONG_BITMASK = -1L

  /**
   * Converts an integer to its corresponding byte array.
   *
   * @param value value to convert
   * @param bitSize value bit size
   * @return corresponding byte array
   */
  def convertInteger(value: Long, bitSize: Int): Array[Byte] = {
    if (value > (LONG_BITMASK >>> (LONG_BITSIZE - bitSize)))
      throw new IllegalArgumentException(s"Number value[$value] exceeds capacity")

    if (bitSize == 8)
      Array[Byte](value.asInstanceOf[Byte])
    else
      (for (idx <- 0 until (bitSize / 8))
        yield ((value >>> (8 * idx)) & 0xFF).asInstanceOf[Byte]).toArray
  }

  /**
   * Reads bytes.
   *
   * Reads until the requested number of bytes or ''EOF'' is reached.
   *
   * @param input stream to read from
   * @param buffer array to store bytes to
   * @param offset array offset
   * @param length number of bytes to read
   * @return number of read bytes
   */
  private def _read(input: InputStream, buffer: Array[Byte], offset: Int,
    length: Int): Int =
  {
    @annotation.tailrec
    def _read(offset: Int, length: Int, acc: Int): Int = {
      if (length <= 0) acc
      else {
        val read = input.read(buffer, offset, length)
        if (read < 0) acc
        else _read(offset + read, length - read, acc + read)
      }
    }

    _read(offset, length, 0)
  }

  /**
   * Reads a byte.
   *
   * @param input stream to read from
   * @return read value
   * @throws EOFException if ''EOF'' was reached before reading value
   */
  def readByte(input: InputStream): Byte = {
    val value = input.read()

    if (value < 0)
      throw new EOFException()
    else
      value.asInstanceOf[Byte]
  }

  /**
   * Reads an integer.
   *
   * @param input stream to read from
   * @param bitSize array length bit size
   * @return read value
   * @throws EOFException if ''EOF'' was reached before reading value
   */
  def readInteger(input: InputStream, bitSize: Int): Long = {
    val idxMax = bitSize / 8
    @annotation.tailrec
    def readInteger(idx: Int, acc: Long): Long = if (idx >= idxMax) acc
      else readInteger(idx + 1,
        acc | ((0xFF & readByte(input).asInstanceOf[Long]) << (8 * idx)))

    readInteger(0, 0)
  }

  /**
   * Reads RAW bytes.
   *
   * @param input stream to read from
   * @param length number of bytes to read
   * @return read value
   * @throws EOFException if ''EOF'' was reached before reading value
   */
  def read(input: InputStream, length: Int): Array[Byte] = {
    val bytes = new Array[Byte](length)

    if (_read(input, bytes, 0, length) != length)
      throw new EOFException()
    else
      bytes
  }

}
