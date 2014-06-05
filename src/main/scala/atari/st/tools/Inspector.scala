package atari.st.tools

import atari.st.disk.Disk
import scala.collection.mutable


object Inspector {

  import Core._

  def inspect() {
    def inspect(map: mutable.Map[String, List[Disk]]) {
      map.toList sortBy(_._2.head.info.name.toLowerCase()) foreach { tuple =>
        val duplicates = sortDuplicates(tuple._2)
        val preferred = duplicates.preferred

        checkFormat(preferred.info)
        for (dup <- duplicates.others ::: duplicates.excluded)
          checkFormat(dup.info)

        if ((options.showDuplicates && (!duplicates.others.isEmpty || !duplicates.excluded.isEmpty)) ||
          options.showUnique)
        {
          if (duplicates.others.isEmpty && duplicates.excluded.isEmpty)
            println(s"Name: ${preferred.info.name}; Image: ${preferred.info}")
          else {
            println(s"Name: ${preferred.info.name}")
            println(s"  Preferred: ${preferred.info}")
          }
          if (!duplicates.others.isEmpty)
            println(s"  Duplicates: ${duplicates.others.map(_.info.path)}")
          if (!duplicates.excluded.isEmpty)
            println(s"  Excluded (for richer disk type): ${duplicates.excluded.map(_.info.path)}")
        }
      }
    }

    findDuplicates()

    if (options.byName) {
      println("")
      println("Disks by name")
      inspect(diskNames)
      println("")
    }

    println("")
    println("Disks by checksum")
    inspect(diskChecksums)
    println("")
  }

}
