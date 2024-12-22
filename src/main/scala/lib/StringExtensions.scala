package lib

import lib.Points.Point

object StringExtensions {
  extension (s: String)
    def toGrid(valid: Char => Boolean = _ => true): Map[Char, Point] =
      s.linesIterator.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.collect { case (char, x) if valid(char) => char -> Point(x, y) }
      }.toMap

}
