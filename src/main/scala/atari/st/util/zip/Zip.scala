package atari.st.util.zip

import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.charset.Charset
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipInputStream}


object Zip {

  /* Note: ZipFile does not (and does not let us) check the CRC of read entry */
  /* XXX - better way (external library ?) to handle zip files without having
   * to read each entry, and still manage CRC checksums etc. ?
   */
  def unzip[T](path: Path, cb: (ZipEntry, InputStream) => (T, Boolean))(implicit charset: Charset): List[T] = {
    /* Note: ZipInputStream does not work with zip files that do not have the
     * header signature right at the start. Some files appear to start with
     * "PK00" and then have the expected "PK\003\004".
     * So check if we get a first entry at the start, or retry at offset 4.
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
