package atari.st.util

import java.nio.file.Path


object Util {

  def findTarget(path: Path): Path = {
    def loop(n: Int): Path = {
      val name: String =
        if (n == 0) path.getFileName.toString
        else {
          val nameSplit = path.getFileName.toString.split("""\.""").toList
          ((nameSplit.head + s"-$n") :: nameSplit.tail).mkString(".")
        }
      val target = path.resolveSibling(name)
      if (!target.toFile.exists) target
      else {
        println(s"Target already exists: $target")
        loop(n + 1)
      }
    }

    loop(0)
  }

}
