import d4p1.*

object d4p2 extends Solution[Int]:
  def generateCrossIndexes(center: Coord, width: Int, height: Int): List[List[Coord]] =
    val nw = List(center.offset(-1), center, center.offset(1))
    val ne = List(center.offset(-1, 1), center, center.offset(1, -1))

    List(nw, nw.reverse, ne, ne.reverse).filter(_.forall(_.inBounds(width, height)))

  override def solve(input: List[String]): Int =
    val masRay = List('M', 'A', 'S')
    locateAllCenters(input, 'A')
      .map(center => generateCrossIndexes(center, input.head.size, input.size))
      .count(checkIndexes(_, input.map(_.toCharArray()), masRay) == 2)
