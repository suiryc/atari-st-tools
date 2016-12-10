package atari.st.util.zip

import java.io.InputStream
import java.util.zip.ZipInputStream


class ZipEntryInputStream(zip: ZipInputStream)
extends InputStream
{

  private var eof = false

  @inline private def doAndEOF[T](f: => T, ifEOF: T = ()): T =
    if (eof) ifEOF
    else {
      val r = f
      eof = available() == 0
      r
    }

  override def available(): Int =
    zip.available()

  override def read(): Int =
    doAndEOF(zip.read(), -1)

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    doAndEOF(zip.read(b, off, len), -1)

  override def skip(n: Long): Long =
    doAndEOF(zip.skip(n), 0)

  override def close(): Unit =
    doAndEOF(zip.closeEntry())

}
