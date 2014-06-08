package atari.st.util

import java.nio.charset.Charset
import java.nio.file.Path
import java.io.{BufferedInputStream, FileInputStream, InputStream, OutputStream}
import java.util.zip.{ZipEntry, ZipInputStream}


object Util {

  def findTarget(path: Path): Path = {
    def loop(n: Int): Path = {
      val name: String =
        if (n == 0) path.getFileName().toString()
        else {
          val nameSplit = path.getFileName().toString().split("""\.""").toList
          ((nameSplit(0) + s"-$n") :: nameSplit.tail).mkString(".")
        }
      val target = path.resolveSibling(name)
      if (!target.toFile.exists) target
      else {
        println(s"Target already exists: ${target}")
        loop(n + 1)
      }
    }

    loop(0)
  }

  /* XXX - better way (external library ?) to handle zip files without having
   * to read each entry, and still manage CRC checksums ?
   */
  /* Note: ZipFile does not (and does not let us) check the CRC of read entry */
  def unzip[T](path: Path, cb: (ZipEntry, InputStream) => (T, Boolean))(implicit charset: Charset): List[T] = {

    /* Note: ZipInputStream does not work with zip files that do not have the
     * header signature right at the start. Some files appear to start with
     * "PK00" instead of the expected "PK\003\004".
     */
    Stream(0, 4) map { offset =>
      val buffered = new BufferedInputStream(new FileInputStream(path.toFile))
      buffered.skip(offset)
      val input = new ZipInputStream(buffered)
      val firstEntry = input.getNextEntry()
      (input, firstEntry)
    } find { tuple =>
      val input = tuple._1
      val firstEntry = tuple._2
      val found = (firstEntry != null)
      if (!found)
        input.close()
      found
    } map { tuple =>
      val input = tuple._1
      val firstEntry = tuple._2

      /* Note: Stream.takeWhile is not what we want since we need to include the
       * last result which denotes to not continue. Moreover since we already
       * got the first element, we would have to define the Stream as such:
       *   (firstEntry #:: Stream.continually(input.getNextEntry))
       * So we have to build the Stream and transform the input (zip entry) to
       * the expected result (T). A nice side effect is that we naturally inject
       * the first entry as such:
       *   stream(firstEntry)
       * XXX - even more natural way to do this ?
       */
      def stream(entry: ZipEntry): Stream[T] = {
        /* Process entry */
        val (t, go) = cb(entry, new ZipEntryInputStream(input))
        /* Continue/stop as needed */
        if (go) {
          val nextEntry = input.getNextEntry
          if (nextEntry == null) t #:: Stream.empty
          else t #:: stream(nextEntry)
        }
        else t #:: Stream.empty
      }

      try {
        stream(firstEntry).toList
      }
      finally {
        input.close()
      }
    } getOrElse(Nil)
  }

}

class ZipEntryInputStream(zip: ZipInputStream)
extends InputStream
{

  private var eof = false

  @inline private def doAndEOF[T](f: => T, ifEOF: T = Unit): T =
    if (eof) ifEOF
    else {
      val r = f
      eof = (available() == 0)
      r
    }

  override def available(): Int =
    zip.available()

  override def read(): Int =
    doAndEOF(zip.read(), -1)

  override def read(b: Array[Byte], off: Int, len: Int) =
    doAndEOF(zip.read(b, off, len), -1)

  override def skip(n: Long) =
    doAndEOF(zip.skip(n), 0)

  override def close() =
    doAndEOF(zip.closeEntry())

}
