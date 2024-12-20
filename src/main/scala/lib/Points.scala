package lib

object Points:

  case class Point(x: Int, y: Int):
    def +(p: Point): Point           = Point(x + p.x, y + p.y)
    def -(p: Point): Point           = Point(x - p.x, y - p.y)
    def *(n: Int): Point             = Point(x * n, y * n)
    def !=(p: Point): Boolean        = p.x != x || p.y != y
    def <=(p: Point): Boolean        = x <= p.x && y <= p.y
    def >=(p: Point): Boolean        = x >= p.x && y >= p.y
    def <=>(p: Point): Boolean       = x >= 0 && y>=0 && this <= p
    def cross(p: Point): Int         = x * p.y - y * p.x
    def left(n: Int = 1): Point      = Point(x - n, y)
    def right(n: Int = 1): Point     = Point(x + n, y)
    def below(n: Int = 1): Point     = Point(x, y + n)
    def above(n: Int = 1): Point     = Point(x, y - n)
    def upright(n: Int = 1): Point   = Point(x + n, y - n)
    def downright(n: Int = 1): Point = Point(x + n, y + n)
    def upleft(n: Int = 1): Point    = Point(x - n, y - n)
    def downleft(n: Int = 1): Point  = Point(x - n, y + n)
    def directions: Seq[Int => Point] =
      Seq(left, right, below, above, upright, downright, upleft, downleft)
    def neighbors: List[Point] =
      List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))
    def corners: List[Point] =
      List(Point(x - 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y - 1), Point(x + 1, y + 1))
    def surroundings: List[Point] = neighbors ++ corners
    def rotate(deg: Int): Point = deg % 360 match
      case 90 | -270  => Point(y, -x)
      case 180 | -180 => Point(-x, -y)
      case -90 | 270  => Point(-y, x)
      case _          => this
    def manhattan(p: Point = Position.zero): Int = (p.x - x).abs + (p.y - y).abs
    def dist(p1: Point, p2: Point): Int =
      if (p1.x - p2.x).abs == 1 && (p1.y - p2.y).abs == 1 then 1
      else (p1.x - p2.x).abs + (p1.y - p2.y).abs
    def directionTo(other: Point): Char = other match
      case Point(x2, y2) if x2 == x && y2 < y => 'N'
      case Point(x2, y2) if x2 == x && y2 > y => 'S'
      case Point(x2, y2) if y2 == y && x2 < x => 'W'
      case Point(x2, y2) if y2 == y && x2 > x => 'E'
      case _ => throw new IllegalArgumentException("Points are equal")
    def mkString = s"$x,$y"  

  object Point:
    val zero: Point = Point(0, 0)

  case class Line(p1: Point, p2: Point):
    val (dx, dy) = ((p2.x - p1.x).sign, (p2.y - p1.y).sign)
    def points: Seq[Point] =
      val max = math.max((p2.x - p1.x).abs, (p2.y - p1.y).abs)
      (0 to max).map(i => Point(p1.x + dx * i, p1.y + dy * i))

  object Position:
    val zero: Point               = Point(0, 0)
    val neighbors: List[Point]    = List(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
    val corners: List[Point]      = List(Point(1, 1), Point(-1, 1), Point(1, -1), Point(-1, -1))
    val surroundings: List[Point] = neighbors ++ corners
    def directions: Map[Char, Point] = Map(
      'E' -> Point(1, 0),
      '>' -> Point(1, 0),
      'R' -> Point(1, 0),
      'W' -> Point(-1, 0),
      '<' -> Point(-1, 0),
      'L' -> Point(-1, 0),
      'N' -> Point(0, -1),
      'U' -> Point(0, -1),
      '^' -> Point(0, -1),
      'D' -> Point(0, 1),
      'S' -> Point(0, 1),
      'v' -> Point(0, 1)
    )

  case class Dir(p: Point, dir: Char):
    def forward(n: Int = 1): Dir = dir match
      case 'U' | 'N' => copy(p = p.copy(y = p.y - n))
      case 'D' | 'S' => copy(p = p.copy(y = p.y + n))
      case 'L' | 'W' => copy(p = p.copy(x = p.x - n))
      case 'R' | 'E' => copy(p = p.copy(x = p.x + n))

    def rotate(clockwise: Boolean = true, n: Int = 0): Dir = dir match // turn and move
      case 'U' => if (clockwise) Dir(Point(p.x + n, p.y), 'R') else Dir(Point(p.x - n, p.y), 'L')
      case 'D' => if (clockwise) Dir(Point(p.x - n, p.y), 'L') else Dir(Point(p.x + n, p.y), 'R')
      case 'L' => if (clockwise) Dir(Point(p.x, p.y - n), 'U') else Dir(Point(p.x, p.y + n), 'D')
      case 'R' => if (clockwise) Dir(Point(p.x, p.y + n), 'D') else Dir(Point(p.x, p.y - n), 'U')

    def rotate(d: Char): Dir = rotate(d == 'R')
    def turn(d: Char): Dir   = copy(dir = d)
    def turn(degrees: Int): Dir =
      val dirs = Seq('U', 'R', 'D', 'L')
      val deg  = (degrees.abs / 90) % 360
      val i    = dirs.indexOf(dir) + (if (degrees < 0) dirs.length - deg else deg)
      copy(dir = dirs(i % dirs.length))

  case class Box(min: Point, max: Point):
    val iterator: Iterator[Point] =
      for
        x <- (min.x to max.x).iterator
        y <- (min.y to max.y).iterator
      yield Point(x, y)
