package atari.st.settings

import atari.st.disk.{ByNameDuplicates, RegexDiskNameFormatter}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.charset.Charset
import java.util
import scala.collection.JavaConversions._
import suiryc.scala.io.PathsEx
import suiryc.scala.settings.BaseConfig


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

  val checkBootSector = value[Boolean]("check.boot-sector")

  val warnUnknownFormat = value[Boolean]("warn.unknown-format")

  val zipCharset = Charset.forName(value[String]("zip.charset"))
  val zipAllowDiskName = value[Boolean]("zip.allow-disk-name")
  val zipAllowExtra = config.getStringList("zip.allow-extra").toList map(_.r)

  val diskNameFormatters = config.getConfigList("disk-name.formatters") flatMap { config =>
    val label = value[String]("label", config)
    val configNormalizers =
      if (config.hasPath("normalizers")) config.getConfigList("normalizers").toList
      else List(config)
    configNormalizers map { config =>
      RegexDiskNameFormatter(
        label,
        value[String]("format", config).r,
        value[String]("normalized", config)
      )
    }
  }

  val outputRoot = PathsEx.get(value[String]("output.root"))
  val outputRelativePreferred = value[String]("output.preferred")
  val outputPreferred = outputRoot.resolve(outputRelativePreferred)
  val outputRelativeAlternatives = value[String]("output.alternatives")
  val outputRelativeOthers = value[String]("output.others")
  val outputOthers = outputRoot.resolve(outputRelativeOthers)
  val outputRelativeConverted = value[String]("output.converted")
  val outputConverted = outputRoot.resolve(outputRelativeConverted)

  val duplicateAllowAlternatives = value[Boolean]("duplicates.allow-alternatives")
  val duplicateBootSectorAllow = value[Boolean]("duplicates.boot-sector.allow")
  val duplicateBootSectorAlternativeImage = option[String]("duplicates.boot-sector.alternative-image")
  val duplicateBootSectorAlternativeSector = option[String]("duplicates.boot-sector.alternative-sector")
  val duplicatesByName = config.getAnyRefList("duplicates.by-name").toList map { el =>
    val dups = el.asInstanceOf[util.ArrayList[util.ArrayList[String]]]
    val keep = dups.get(0).map(_.toUpperCase)
    val drop = dups.get(1).map(_.toUpperCase).toSet
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
