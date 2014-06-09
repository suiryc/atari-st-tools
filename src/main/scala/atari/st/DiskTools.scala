package atari.st

import atari.st.disk.DiskType
import atari.st.settings.Settings
import atari.st.tools.{Converter, Deduplicator, Normalizer, Tester}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Path, Paths}
import scala.util.matching.Regex


object DiskTools extends App {

  object AppMode extends Enumeration {
    val convert = Value
    val deduplicate = Value
    val inspect = Value
    val normalize = Value
    val test = Value
  }

  case class AppOptions(
    allowByName: Boolean = false,
    checkBootSector: Boolean = Settings.core.checkBootSector,
    dryRun: Boolean = false,
    duplicateBootSectorAllow: Boolean = Settings.core.duplicateBootSectorAllow,
    duplicateBootSectorAlternativeImage: Option[String] = Settings.core.duplicateBootSectorAlternativeImage,
    duplicateBootSectorAlternativeSector: Option[String] = Settings.core.duplicateBootSectorAlternativeSector,
    mode: AppMode.Value = AppMode.test,
    outputPreferred: Path = Settings.core.outputPreferred,
    outputOthers: Path = Settings.core.outputOthers,
    outputConverted: Path = Settings.core.outputConverted,
    outputType: DiskType.Value = DiskType.MSA,
    showDuplicates: Boolean = false,
    showUnique: Boolean = false,
    sources: List[Path] = Nil,
    verbose: Int = 0,
    warnUnknownFormat: Boolean = Settings.core.warnUnknownFormat,
    zip: Boolean = false,
    zipAllowDiskName: Boolean = Settings.core.zipAllowDiskName,
    zipAllowExtra: List[Regex] = Settings.core.zipAllowExtra,
    zipAllowExtraOverriden: Boolean = false,
    zipCharset: Charset = Settings.core.zipCharset
  )


  println(s"********** Started: ${new java.util.Date}")
  println(s"Args: ${args.toList}")

  val parser = new scopt.OptionParser[AppOptions]("DiskTools") {
    help("help") text("print help")
    val allowByName = () => opt[Unit]("allow-by-name") text("allow duplicates by name (but not by checksum)") action { (_, c) =>
      c.copy(allowByName = true)
    }
    opt[Unit]("check-boot-sector") text("check boot sector") action { (_, c) =>
      c.copy(checkBootSector = true)
    }
    opt[Unit]("no-check-boot-sector") text("do not check boot sector") action { (_, c) =>
      c.copy(checkBootSector = false)
    }
    val dryRun = () => opt[Unit]("dry-run") text("do not modify sources") action { (_, c) =>
      c.copy(dryRun = true)
    }
    opt[String]("output") text("output root path") action { (v, c) =>
      val outputRoot = Paths.get(v)
      c.copy(
        outputPreferred = outputRoot.resolve(Settings.core.outputRelativePreferred),
        outputOthers = outputRoot.resolve(Settings.core.outputRelativeOthers),
        outputConverted = outputRoot.resolve(Settings.core.outputRelativeConverted)
      )
    }
    opt[Unit]('v', "verbose") text("increase verbosity level") action { (_, c) =>
      c.copy(verbose = c.verbose + 1)
    }
    opt[Unit]("warn-unknown-format") text("warn on unknown disk format") action { (_, c) =>
      c.copy(warnUnknownFormat = true)
    }
    opt[Unit]("no-warn-unknown-format") text("do not warn on unknown disk format") action { (_, c) =>
      c.copy(warnUnknownFormat = false)
    }
    val zip = () => opt[Unit]("zip") text("zip each disk image") action { (_, c) =>
      c.copy(zip = true)
    }
    opt[Unit]("zip-allow-disk-name") text("allow zip to contain another file which name is the disk") action { (_, c) =>
      c.copy(zipAllowDiskName = true)
    }
    opt[Unit]("no-zip-allow-disk-name") text("do not allow zip to contain another file which name is the disk") action { (_, c) =>
      c.copy(zipAllowDiskName = false)
    }
    opt[String]("zip-allow-extra") text("allow extra zip entries name") unbounded() action { (v, c) =>
      val zipAllowExtra =
        if (!c.zipAllowExtraOverriden) List(v.r)
        else c.zipAllowExtra :+ v.r
      c.copy(zipAllowExtra = zipAllowExtra, zipAllowExtraOverriden = true)
    }
    val zipCharset = () => opt[String]("zip-charset") text("zip entries charset (if not UTF8)") minOccurs(
      if (Settings.core.zipCharset.compareTo(StandardCharsets.UTF_8) == 0) 1 else 0
    ) action { (v, c) =>
      c.copy(zipCharset = Charset.forName(v))
    }
    val source = () => arg[String]("source...") text("files/folders to process") unbounded() action { (v, c) =>
      c.copy(sources = c.sources :+ Paths.get(v))
    }

    cmd("convert") text("convert disk image") action { (_, c) =>
      c.copy(mode = AppMode.convert)
    } children(
      dryRun(),
      opt[String]("to") text("output type") action { (v, c) =>
        c.copy(outputType = DiskType(v))
      },
      zip(),
      zipCharset(),
      source()
    )

    cmd("deduplicate") text("deduplicate") action { (_, c) =>
      c.copy(mode = AppMode.deduplicate)
    } children(
      allowByName(),
      dryRun(),
      zipCharset(),
      source()
    )

    cmd("inspect") text("inspect disks") action { (_, c) =>
      c.copy(mode = AppMode.inspect)
    } children(
      allowByName(),
      opt[Unit]("show-duplicates") text("show duplicate disks") action { (_, c) =>
        c.copy(showDuplicates = true)
      },
      opt[Unit]("show-unique") text("show unique disks") action { (_, c) =>
        c.copy(showUnique = true)
      },
      zipCharset(),
      source()
    )

    cmd("normalize") text("normalize") action { (_, c) =>
      c.copy(mode = AppMode.normalize)
    } children(
      dryRun(),
      zip(),
      zipCharset(),
      source()
    )

    cmd("test") text("test") action { (_, c) =>
      c.copy(mode = AppMode.test)
    }
  }

  val options = parser.parse(args, AppOptions()) getOrElse { sys.exit(1) }
  println(s"Options:")
  /* XXX - scala way to do this ? */
  for (field <- options.getClass().getDeclaredFields().toList.sortBy(_.getName())) {
    val fieldName = field.getName()
    val fieldValue = options.getClass().getMethod(fieldName).invoke(options)
    println(s"  ${fieldName}: ${fieldValue}")
  }

  options.mode match {
    case AppMode.convert =>
      convert()

    case AppMode.deduplicate =>
      deduplicate()

    case AppMode.inspect =>
      inspect()

    case AppMode.normalize =>
      normalize()

    case AppMode.test =>
      test()
  }
  println(s"********** Ended: ${new java.util.Date}")

  def convert() {
    Converter.convert()
  }

  def deduplicate() {
    Deduplicator.deduplicate(inspect = false)
  }

  def inspect() {
    Deduplicator.deduplicate(inspect = true)
  }

  def normalize() {
    Normalizer.normalize()
  }

  def test() {
    Tester.test()
  }

}
