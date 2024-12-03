import java.nio.file.DirectoryStream

object d2p1 extends Solution[Int]:
  enum Direction:
    case UP
    case DOWN
    case NOWHERE

  object Direction:
    def fromDelta(delta: Int): Direction =
      if delta < 0 then Direction.DOWN else Direction.UP

  case class ReportSafetyIndicator(direction: Direction, monotonic: Boolean, safeDelta: Boolean):
    def ingestSignalChunk(chunk: List[Int]): ReportSafetyIndicator =
      val delta = chunk match
        case a :: b :: Nil => a - b
        case _             => 0

      val absDelta = delta.abs
      this.and(Direction.fromDelta(delta), absDelta >= 1 && absDelta <= 3)

    def and(currentDirection: Direction, safeDelta: Boolean): ReportSafetyIndicator =
      ReportSafetyIndicator(
        currentDirection,
        ReportSafetyIndicator.isMonotonicDirection(this, currentDirection),
        this.safeDelta && safeDelta
      )

    def isSafe = this.monotonic && this.safeDelta

  object ReportSafetyIndicator:
    def isMonotonicDirection(indicator: ReportSafetyIndicator, direction: Direction): Boolean =
      indicator.direction == Direction.NOWHERE || (indicator.monotonic && (indicator.direction == direction))

    val safe = ReportSafetyIndicator(Direction.NOWHERE, true, true)

  def parseReports(input: List[String]): List[List[Int]] =
    def parseReport(report: String): List[Int] = report.split(" ").map(_.toInt).toList
    input.map(parseReport)

  def isSafe(report: List[Int]): Boolean =
    report
      .sliding(2, 1)
      .foldLeft(ReportSafetyIndicator.safe)((indicator, chunk) => indicator.ingestSignalChunk(chunk))
      .isSafe

  override def solve(input: List[String]): Int =
    parseReports(input).count(isSafe)
