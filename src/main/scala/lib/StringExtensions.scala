package lib

import lib.Points.Point

object StringExtensions {
  extension (s: String)
    def toGrid: Map[Char, Point] =
      s.linesIterator.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.collect { case (char, x) if char != ' ' => char -> Point(x, y) }
      }.toMap

}
