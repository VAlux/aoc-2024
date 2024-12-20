import scala.annotation.static

object d4p1 extends Solution[Int]:
  case class Coord(row: Int, column: Int):
    def offset(delta: Int): Coord       = Coord(row + delta, column + delta)
    def offset(dr: Int, dc: Int): Coord = Coord(row + dr, column + dc)
    def offsetRow(dr: Int): Coord       = Coord(row + dr, column)
    def offsetColumn(dc: Int): Coord    = Coord(row, column + dc)

    def inBounds(width: Int, height: Int): Boolean =
      row >= 0 && column >= 0 && row < height && column < width

  def generateStarIndexes(center: Coord, width: Int, height: Int): List[List[Coord]] =
    val w  = (0 to 3).map(index => center.offsetColumn(-index)).toList
    val e  = (0 to 3).map(index => center.offsetColumn(index)).toList
    val n  = (0 to 3).map(index => center.offsetRow(-index)).toList
    val s  = (0 to 3).map(index => center.offsetRow(index)).toList
    val nw = (0 to 3).map(index => center.offset(index, -index)).toList
    val ne = (0 to 3).map(index => center.offset(index)).toList
    val se = (0 to 3).map(index => center.offset(-index, index)).toList
    val sw = (0 to 3).map(index => center.offset(-index)).toList

    List(w, e, n, s, nw, ne, se, sw).filter(_.forall(_.inBounds(width, height)))

  def locateAllCenters(input: List[String], targetCharacter: Char): List[Coord] =
    input.zipWithIndex.flatMap: (line, rowIndex) =>
      line
        .toCharArray()
        .zipWithIndex
        .filter((char, _) => char == targetCharacter)
        .map((_, columnIndex) => Coord(rowIndex, columnIndex))
        .toList

  def checkIndexes(starIndexes: List[List[Coord]], input: List[Array[Char]], target: List[Char]): Int =
    starIndexes
      .map(starRay => starRay.map(coord => input(coord.row)(coord.column)))
      .count(_ == target)

  override def solve(input: List[String]): Int =
    val xmasRay = List('X', 'M', 'A', 'S')
    locateAllCenters(input, 'X')
      .map(center => generateStarIndexes(center, input.head.size, input.size))
      .map(checkIndexes(_, input.map(_.toCharArray()), xmasRay))
      .sum
