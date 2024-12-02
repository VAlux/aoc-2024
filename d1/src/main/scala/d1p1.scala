object d1p1 extends Solution[Int]:
  case class LocationIdLists(left: List[Int], right: List[Int]):
    def sorted: LocationIdLists = LocationIdLists(left.sortWith(_ - _ < 0), right.sortWith(_ - _ < 0))
    def deltas: List[Int]       = left.zip(right).map((l, r) => Math.abs(l - r))
    def simScore: Int           = left.map(item => right.count(_ == item) * item).sum

  object LocationIdLists:
    val empty = LocationIdLists(List.empty, List.empty)

  def parseLists(input: List[String]): LocationIdLists =
    input.map(_.split(" ").filter(!_.isBlank()).map(_.toInt).toList).transpose match
      case left :: right :: Nil => LocationIdLists(left, right)
      case _                    => LocationIdLists.empty

  override def solve(input: List[String]): Int =
    parseLists(input).sorted.deltas.sum
