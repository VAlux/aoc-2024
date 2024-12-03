import d2p1.*

object d2p2 extends Solution[Int]:
  def generateReportVariants(report: List[Int]): List[List[Int]] =
    (0 until report.size).map(index => report.take(index) ++ report.drop(index + 1)).toList

  override def solve(input: List[String]): Int =
    parseReports(input).count(report => generateReportVariants(report).exists(isSafe))
