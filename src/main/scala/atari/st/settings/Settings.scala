package atari.st.settings

import atari.st.disk.{ByNameDuplicates, RegexDiskNameFormatter}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.charset.Charset
import java.nio.file.Path
import java.util
import scala.collection.JavaConverters._
import suiryc.scala.io.PathsEx
import suiryc.scala.settings.BaseConfig
import scala.util.matching.Regex


object Settings {

  protected val confPath = "atari-st-tools"

  /** Core settings. */
  val core = new Settings(ConfigFactory.load().getConfig(confPath)/*,
    Preferences.userRoot.node("suiryc.atari-st-tools").node(confPath)*/)

  def load() {
    /* Settings are automatically loaded by accessing this object for the
     * first time.
     */
  }

}

class Settings(
  override val config: Config/*,
  prefs: Preferences*/
) extends BaseConfig(config)
/* extends BaseSettings(config, prefs)*/
{

  import BaseConfig._

  val checkBootSector: Boolean = value[Boolean]("check.boot-sector")

  val warnUnknownFormat: Boolean = value[Boolean]("warn.unknown-format")

  val zipCharset: Charset = Charset.forName(value[String]("zip.charset"))
  val zipAllowDiskName: Boolean = value[Boolean]("zip.allow-disk-name")
  val zipAllowExtra: List[Regex] = config.getStringList("zip.allow-extra").asScala.toList map(_.r)

  val diskNameFormatters: List[RegexDiskNameFormatter] = config.getConfigList("disk-name.formatters").asScala.toList.flatMap { config =>
    val label = value[String]("label", config)
    val configNormalizers =
      if (config.hasPath("normalizers")) config.getConfigList("normalizers").asScala.toList
      else List(config)
    configNormalizers.map { config =>
      RegexDiskNameFormatter(
        label,
        value[String]("format", config).r,
        value[String]("normalized", config)
      )
    }
  }

  val outputRoot: Path = PathsEx.get(value[String]("output.root"))
  val outputRelativePreferred: String = value[String]("output.preferred")
  val outputPreferred: Path = outputRoot.resolve(outputRelativePreferred)
  val outputRelativeAlternatives: String = value[String]("output.alternatives")
  val outputRelativeOthers: String = value[String]("output.others")
  val outputOthers: Path = outputRoot.resolve(outputRelativeOthers)
  val outputRelativeConverted: String = value[String]("output.converted")
  val outputConverted: Path = outputRoot.resolve(outputRelativeConverted)

  val duplicateAllowAlternatives: Boolean = value[Boolean]("duplicates.allow-alternatives")
  val duplicateBootSectorAllow: Boolean = value[Boolean]("duplicates.boot-sector.allow")
  val duplicateBootSectorAlternativeImage: Option[String] = option[String]("duplicates.boot-sector.alternative-image")
  val duplicateBootSectorAlternativeSector: Option[String] = option[String]("duplicates.boot-sector.alternative-sector")
  val duplicatesByName: Map[String, ByNameDuplicates] = config.getAnyRefList("duplicates.by-name").asScala.toList map { el =>
    val dups = el.asInstanceOf[util.ArrayList[util.ArrayList[String]]]
    val keep = dups.get(0).asScala.map(_.toUpperCase)
    val drop = dups.get(1).asScala.map(_.toUpperCase).toSet
    val byNameDups = new ByNameDuplicates(keep.headOption.getOrElse(""), keep.toSet, drop)
    (keep ++ drop).map { checksum =>
      (checksum, byNameDups)
    }.toMap
  } reduceLeft(_ ++ _)

  if (duplicateBootSectorAllow &&
    duplicateBootSectorAlternativeImage.isEmpty &&
    duplicateBootSectorAlternativeSector.isEmpty)
    throw new Exception("When allowing duplicates with different boot sector, either alternative-image or alternative-sector option must be set")

}
