package atari.st.settings

import atari.st.disk.RegexDiskNameFormatter
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.charset.Charset
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

  val diskNameFormatters = config.getConfigList("disk-name.formatters") map { config =>
    RegexDiskNameFormatter(
      value[String]("label", config),
      value[String]("format", config).r,
      value[String]("normalized", config)
    )
  }

  val outputRoot = PathsEx.get(value[String]("output.root"))
  val outputRelativePreferred = value[String]("output.preferred")
  val outputPreferred = outputRoot.resolve(outputRelativePreferred)
  val outputRelativeOthers = value[String]("output.others")
  val outputOthers = outputRoot.resolve(outputRelativeOthers)
  val outputRelativeConverted = value[String]("output.converted")
  val outputConverted = outputRoot.resolve(outputRelativeConverted)

}
