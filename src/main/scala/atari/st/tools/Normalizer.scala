package atari.st.tools

import atari.st.disk.{DiskInfo, DiskNameFormatter, DiskType}
import atari.st.settings.Settings
import atari.st.util.Util
import java.io.{
  BufferedInputStream,
  BufferedOutputStream,
  ByteArrayOutputStream,
  FileInputStream,
  FileOutputStream
}
import java.nio.file.{Files, Path}
import java.nio.file.attribute.{BasicFileAttributes, BasicFileAttributeView}
import java.util.zip.{ZipEntry, ZipException, ZipFile, ZipOutputStream}


object Normalizer {

  import Core._

  def normalize() {
    def normalize(source: Path, root: Path, files: List[Path]) {
      if (!files.isEmpty) {
        if (Files.isDirectory(source)) {
          println(s"Processing files in ${source} ...")
        }
        else if (Files.exists(source)) {
          println(s"Processing file ${source} ...")
        }
      }
      files sortBy(_.toString.toLowerCase) foreach(process)
    }

    findFiles(normalize)
  }

  def process(path: Path) {
    val filename = path.getFileName.toString
    if (DiskInfo.extension(filename).toLowerCase == "zip") {
      processZip(path)
    }
    else {
      val normalized = normalizeFilename(filename)
      val normalizedPath =
        if (filename != normalized) {
          val normalizedPath = Util.findTarget(path.resolveSibling(normalized))
          if (!options.dryRun)
            Files.move(path, normalizedPath)
          println(s"Path[$path] filename renamed[${normalizedPath.getFileName}]")
          normalizedPath
        }
        else path

      if (options.zip) {
        println(s"Zip disk[$normalizedPath]")
        zipPath(normalizedPath)
      }
    }
  }

  def processZip(path: Path) {
    try {
      val zip = new ZipFile(path.toFile, options.zipCharset)

      val entries = DiskInfo.zipEntries(zip, options.zipAllowDiskName, options.zipAllowExtra)
      if ((entries.disks.length > 0) && (entries.extraUnknown.length == 0)) {
        val normalizedDisks = entries.disks map { info =>
          val entry = info.entry
          val normalized = normalizeFilename(entry.getName())
          (entry, normalized)
        }
        val diskName = DiskInfo.atomicName(normalizedDisks.head._2)
        val normalizedExtra = entries.extraAllowed map { entry =>
          val normalized = s"${diskName}-${DiskInfo.filename(entry.getName()).toLowerCase}"
          (entry, normalized)
        }
        val normalizedEntries = normalizedDisks ::: normalizedExtra
        val needUnzip = !options.zip || (normalizedEntries.length > 1) || normalizedEntries.exists { tuple =>
          tuple._1.getName != tuple._2
        }
        val currentFilename = path.getFileName.toString
        val normalizedZipName = s"${diskName}.zip"

        if (needUnzip) {
          unzipPath(path, zip, normalizedEntries)
        }
        else if (currentFilename != normalizedZipName) {
          /* Note: take into account names collisions */
          val target = path.resolveSibling(normalizedZipName)
          val collision =
            if (currentFilename.startsWith(s"${diskName}-") && currentFilename.endsWith(".zip")) {
              try {
                val idx = currentFilename.substring(diskName.length + 1, currentFilename.length - 4).toInt
                val collisions = target :: (1 until idx).map(idx => path.resolveSibling(s"${diskName}-${idx}.zip")).toList
                collisions.forall(Files.exists(_))
              }
              catch {
                case e: Throwable =>
                  false
              }
            }
            else false

          if (!collision) {
            val normalizedPath = Util.findTarget(target)
            if (!options.dryRun)
              Files.move(path, normalizedPath)
            println(s"Archive[$path] filename renamed[${normalizedPath.getFileName}]")
          }
          /* else: name is actually valid due to collisions */
          else if (options.verbose > 0) {
            println(s"Archive[${path}] is OK")
          }
        }
        /* else: all ok */
        else if (options.verbose > 0) {
          println(s"Archive[${path}] is OK")
        }
      }
      else if (entries.disks.length > 0) {
        println(s"Archive[$path] disk images[${entries.disks}] not processed due to extra files[${entries.extraUnknown}]")
      }
      else if (options.verbose > 0) {
        println(s"Archive[$path] does not contain disk images")
      }

      zip.close()
    }
    catch {
      case ex: Throwable =>
        println(s"Error with file[$path]: ${ex.getMessage()}")
        ex.printStackTrace()
    }
  }

  def unzipPath(path: Path, zip: ZipFile, entries: List[(ZipEntry, String)]) {
    var seen = List[ZipEntry]()
    var created = List[Path]()
    try Util.unzip(path, { (_entry, input) =>
      entries.find(_._1.getName() == _entry.getName()) foreach { tuple =>
        val entry = tuple._1
        seen :+= entry
        val normalized = tuple._2
        val target = path.resolveSibling(normalized)
        val normalizedPath = Util.findTarget(target)

        /* XXX - configurable input/output buffer size ? */
        val output =
          if (options.dryRun) new ByteArrayOutputStream()
          else new BufferedOutputStream(new FileOutputStream(normalizedPath.toFile))
        created :+= normalizedPath
        val (_, size) = try {
          val r = Util.transfer(input, output)
          output.flush()
          output.close()

          if (!options.dryRun) {
            if (entry.getTime() > 0)
              normalizedPath.toFile.setLastModified(entry.getTime())
            val attrView = Files.getFileAttributeView(normalizedPath, classOf[BasicFileAttributeView])
            attrView.setTimes(entry.getLastModifiedTime(), entry.getLastAccessTime(), entry.getCreationTime())
            /* XXX - keep access (read, write, execute) rights ? */
          }

          r
        }
        catch {
          case ex: Throwable =>
            println(s"Archive[$path] entry[${entry.getName()}] error: ${ex.getMessage}")
            throw ex
        }

        println(s"Archive[$path] entry[${entry.getName()}] extracted[${normalizedPath.getFileName}] size[${size}]")
      }
      (Unit, true)
    }) catch {
      case ex: Throwable =>
        println(s"Archive[${path}] entries not processed: ${entries}")
        created foreach { path =>
          if (!options.dryRun)
            path.toFile.delete()
          if (options.verbose > 0)
            println(s"Delete extracted path[${path}]")
        }
        throw ex
    }

    /* Sanity check, since we are using two different Zip readers */
    if (seen.length != entries.length) {
      val ex = new ZipException("ZipFile/ZipInputStream mismatch")
      val notSeen =
        entries map(_._1) filterNot { entry =>
          seen.exists(_.getName() == entry.getName())
        }
      println(s"Archive[${path}] entries not processed: ${notSeen}")
      throw ex
    }

    if (!options.dryRun)
      path.toFile.delete()
    if (options.verbose > 0)
      println(s"Archive[${path}] deleted")

    if (options.zip) {
      created foreach { path =>
        DiskInfo.imageType(DiskInfo.extension(path.getFileName.toString)) match {
          case DiskType.Unknown =>
            /* not a disk image, nothing else to do */

          case _ =>
            println(s"Zip disk[$path]")
            zipPath(path)
        }
      }
    }
  }

  def zipPath(path: Path) {
    if (!options.dryRun) {
      val filename = path.getFileName().toString
      val target = Util.findTarget(path.resolveSibling(s"${DiskInfo.atomicName(filename)}.zip"))
      val input = new BufferedInputStream(new FileInputStream(path.toFile))
      val output = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(target.toFile)))
      val entry = new ZipEntry(filename)

      val attr = Files.readAttributes(path, classOf[BasicFileAttributes])
      if (attr.creationTime().toMillis() > 0)
        entry.setCreationTime(attr.creationTime())
      if (attr.lastModifiedTime().toMillis() > 0) {
        entry.setLastModifiedTime(attr.lastModifiedTime())
        entry.setTime(attr.lastModifiedTime().toMillis())
      }
      else {
        entry.setTime(path.toFile().lastModified())
      }
      if (attr.lastAccessTime().toMillis() > 0)
        entry.setLastAccessTime(attr.lastAccessTime())
      /* XXX - keep access (read, write, execute) rights ? */

      output.putNextEntry(entry)
      Util.transfer(input, output)
      output.closeEntry()
      output.finish()
      output.flush()
      output.close()

      path.toFile.delete()
    }
  }

  def normalizeFilename(name: String) = {
    val diskName = DiskInfo.atomicName(name)
    val extension = DiskInfo.extension(name).toLowerCase
    val formatter = nameFormatter(diskName)

    val normalized = s"${formatter.normalize(diskName)}.${extension}"
    if ((options.verbose > 0) && (normalized != name))
      println(s"Filename[${name}] normalized[${normalized}] by formatter[${formatter.label}]")
    normalized
  }

  def nameFormatter(name: String) =
    Settings.core.diskNameFormatters find { formatter =>
      formatter.matches(name)
    } getOrElse(DiskNameFormatter.lowerCase)

}
