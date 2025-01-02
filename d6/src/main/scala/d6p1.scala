import org.xml.sax.helpers.LocatorImpl
object d6p1 extends Solution[Int]:
  import Direction.*
  import Entity.*

  enum Direction:
    case N
    case S
    case E
    case W

  object Direction:
    def rotateCW(current: Direction): Direction = current match
      case N => E
      case E => S
      case S => W
      case W => N

  enum Entity:
    case Obstacle
    case Guard
    case Trace
    case Empty

  object Entity:
    def parse(character: Char): Entity = character match
      case '#'                   => Obstacle
      case '>' | '<' | '^' | 'v' => Guard
      case _                     => Empty

  case class Location(entity: Entity, row: Int, column: Int):
    def samePosition(other: Location): Boolean =
      this.row == other.row && this.column == other.column

  object Location:
    def empty = Location(Empty, 0, 0)

  case class LocationIndex(rowIndex: Map[Int, List[Location]], columnIndex: Map[Int, List[Location]])

  def parseWorld(input: List[String]): List[Location] =
    input.zipWithIndex.flatMap: (line, row) =>
      line.zipWithIndex.map: (character, column) =>
        Location(parse(character), row, column)

  def buildIndex(obstacles: List[Location]): LocationIndex =
    val rowIndex    = obstacles.groupBy(_.row)
    val columnIndex = obstacles.groupBy(_.column)
    LocationIndex(rowIndex, columnIndex)

  def printWorld(
    world: List[Location],
    guardLocation: Location,
    guardOrientation: Direction,
    currentTrace: Set[Location]
  ): List[Char] =
    def printGuard(): Char = guardOrientation match
      case N => '^'
      case S => 'v'
      case E => '>'
      case W => '<'

    world.map: location =>
      if location.samePosition(guardLocation) then printGuard()
      else if currentTrace.exists(_.samePosition(location)) then 'X'
      else
        location.entity match
          case Obstacle => '#'
          case Guard    => 'S'
          case Trace    => 'X'
          case Empty    => '.'

  class GuardWalker(width: Int, height: Int):
    def getPathSegment(guard: Location, direction: Direction, obstacle: Location): Seq[Location] =
      direction match
        case N => (obstacle.row + 1 until guard.row).map(row => Location(Trace, row, guard.column))
        case S => (guard.row until obstacle.row).map(row => Location(Trace, row, guard.column))
        case E => (guard.column until obstacle.column).map(column => Location(Trace, guard.row, column))
        case W => (obstacle.column + 1 until guard.column).map(column => Location(Trace, guard.row, column))

    def nextGuardLocation(guard: Location, direction: Direction, obstacle: Location): Location =
      direction match
        case N => Location(Guard, obstacle.row + 1, guard.column)
        case S => Location(Guard, obstacle.row - 1, guard.column)
        case E => Location(Guard, guard.row, obstacle.column - 1)
        case W => Location(Guard, guard.row, obstacle.column + 1)

    def step(guard: Location, direction: Direction, obstacle: Location): (Set[Location], Location) =
      val segment       = getPathSegment(guard, direction, obstacle)
      val guardLocation = nextGuardLocation(guard, direction, obstacle)
      (segment.toSet, guardLocation)

    def getClosestObstacle(guard: Location, direction: Direction, index: LocationIndex): Option[Location] =
      def getObstacleLocation(
        targetIndex: Map[Int, List[Location]],
        key: Int,
        filterF: (Location) => Boolean,
        sortF: (Location) => Int
      ): Option[Location] =
        targetIndex.get(key).flatMap(_.filter(filterF).sortBy(sortF).headOption)

      direction match
        case N => getObstacleLocation(index.columnIndex, guard.column, loc => guard.row > loc.row, loc => -loc.row)
        case S => getObstacleLocation(index.columnIndex, guard.column, loc => guard.row < loc.row, loc => loc.row)
        case E => getObstacleLocation(index.rowIndex, guard.row, loc => guard.column < loc.column, loc => loc.column)
        case W => getObstacleLocation(index.rowIndex, guard.row, loc => guard.column > loc.column, loc => -loc.column)

    def getBorderSegment(guard: Location, direction: Direction): Seq[Location] =
      direction match
        case N => getPathSegment(guard, direction, Location(Obstacle, -1, guard.column))
        case S => getPathSegment(guard, direction, Location(Obstacle, height, guard.column))
        case E => getPathSegment(guard, direction, Location(Obstacle, guard.row, width))
        case W => getPathSegment(guard, direction, Location(Obstacle, guard.row, -1))

    def walk(
      guard: Location,
      direction: Direction,
      index: LocationIndex,
      acc: Set[Location] = Set.empty
    ): Set[Location] =
      getClosestObstacle(guard, direction, index) match
        case None           => acc ++ getBorderSegment(guard, direction).toSet
        case Some(obstacle) =>
          val (pathSegment, guardLocation) = step(guard, direction, obstacle)
          walk(guardLocation, Direction.rotateCW(direction), index, acc ++ pathSegment)

  override def solve(input: List[String]): Int =
    val width    = input(0).size
    val height   = input.size
    val world    = parseWorld(input)
    val index    = buildIndex(world.filter(_.entity == Obstacle))
    val guard    = world.find(_.entity == Guard)
    val walker   = GuardWalker(input(0).size, input.size)
    val trace    = guard.map(guard => walker.walk(guard, Direction.N, index)).getOrElse(Set.empty)
    val count    = trace.size
    val newWorld = printWorld(world, guard.getOrElse(Location.empty), N, trace)
      .sliding(height, height)
      .map(a => a.mkString)
      .mkString("\n")

    println(newWorld)
    count
