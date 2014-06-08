package atari.st.disk

import scala.util.matching.Regex


abstract class DiskNameFormatter(val label: String) {

  def matches(name: String): Boolean

  def normalize(name: String): String

}

object DiskNameFormatter {

  val lowerCase = new DiskNameFormatter("lowercase") {
    override def matches(name: String) = true
    override def normalize(name: String) = name.toLowerCase
  }

}

case class RegexDiskNameFormatter(
  override val label: String,
  format: Regex,
  normalized: String
) extends DiskNameFormatter(label)
{

  def matches(name: String) =
    format.pattern.matcher(name).matches

  def normalize(name: String) =
    format.findFirstMatchIn(name) map { m =>
      val version = m.group(1).toInt
      val revision = m.group(2)
      normalized.format(version, revision).toLowerCase
    } getOrElse(name.toLowerCase)

}
