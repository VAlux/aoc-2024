import java.nio.file.DirectoryStream

object d2p1 extends Solution[Int]:
  enum Direction:
    case UP
    case DOWN
    case NOWHERE

  object Direction:
    def fromDelta(delta: Int): Direction =
      if delta < 0 then Direction.DOWN else Direction.UP

  case class SafetyIndicator(direction: Direction, monotonic: Boolean, safeDelta: Boolean):
    val isSafe = this.monotonic && this.safeDelta

    def ingestSignalChunk(chunk: List[Int]): SafetyIndicator =
      val delta = chunk match
        case a :: b :: Nil => a - b
        case _             => 0

      val absDelta = delta.abs
      this.and(Direction.fromDelta(delta), absDelta >= 1 && absDelta <= 3)

    def and(currentDirection: Direction, safeDelta: Boolean): SafetyIndicator =
      SafetyIndicator(
        currentDirection,
        SafetyIndicator.isMonotonicDirection(this, currentDirection),
        this.safeDelta && safeDelta
      )

  object SafetyIndicator:
    def isMonotonicDirection(indicator: SafetyIndicator, direction: Direction): Boolean =
      indicator.direction == Direction.NOWHERE || (indicator.monotonic && (indicator.direction == direction))

    val safe   = SafetyIndicator(Direction.NOWHERE, true, true)
    val unsafe = SafetyIndicator(Direction.NOWHERE, false, false)

  def parseReports(input: List[String]): List[List[Int]] =
    def parseReport(report: String): List[Int] = report.split(" ").map(_.toInt).toList
    input.map(parseReport)

  def isSafe(report: List[Int]): Boolean =
    def ingestSignalChunk(indicator: SafetyIndicator, chunk: List[Int]): SafetyIndicator =
      if indicator.isSafe
      then indicator.ingestSignalChunk(chunk)
      else SafetyIndicator.unsafe

    report.sliding(2, 1).foldLeft(SafetyIndicator.safe)(ingestSignalChunk).isSafe

  override def solve(input: List[String]): Int =
    parseReports(input).count(isSafe)
