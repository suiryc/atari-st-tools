package atari.st.tools

import atari.st.disk.DiskFormat


object Tester {

  def test() {
    for (sides <- 1 to 2) {
      for (tracks <- 78 to 84) {
        for (sectorsPerTrack <- 8 to 12) {
          val sectors = sectorsPerTrack * tracks * sides
          val format = DiskFormat(sectors, tracks, sectorsPerTrack, sides)
          val guessed = DiskFormat(format.size)
          if (format == guessed)
            println(s"${format.size / 1024}kiB: $guessed")
          else
            println(s"Wrong guess $guessed, expected format")
        }
      }
    }
  }

}
