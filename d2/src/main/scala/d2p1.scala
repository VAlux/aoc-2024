object d2p1 extends Solution[Int]:
  case class ReportSafetyIndicator(monotonic: Boolean, safeDelta: Boolean):
    def and(monotonic: Boolean, safeDelta: Boolean): ReportSafetyIndicator =
      ReportSafetyIndicator(this.monotonic && monotonic, this.safeDelta && safeDelta)

    def isSafe = this.monotonic && this.safeDelta

  object ReportSafetyIndicator:
    val safe = ReportSafetyIndicator(true, true)

  def parseReports(input: List[String]): List[List[Int]] =
    def parseReport(report: String): List[Int] = report.split(" ").map(_.toInt).toList
    input.map(parseReport)

  def isSafe(report: List[Int]): Boolean =
    report
      .sliding(2, 1)
      .foldLeft(ReportSafetyIndicator.safe) { (acc, current) =>
        val delta = current match
          case a :: b :: Nil => Math.abs(a - b)
          case _             => 0

        acc.and(delta >= 1 && delta <= 3, true)
      }
      .isSafe

  override def solve(input: List[String]): Int =
    val reports = parseReports(input)
    0
