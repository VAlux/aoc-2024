import d1p1.*
object d1p2 extends Solution[Int]:
  override def solve(input: List[String]): Int =
    parseLists(input).simScore
