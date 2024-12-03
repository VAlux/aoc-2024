object d3p1 extends Solution[Int]:
  val mulRegex = s"mul\\(\\d+,\\d+\\)".r

  enum Operation:
    case MUL
    case NOP

  case class Instruction(operation: Operation, operands: List[String])
  object Instruction:
    val NOP = Instruction(Operation.NOP, List.empty)

  def parseInstruction(input: String): Instruction =
    Instruction(
      Operation.MUL,
      input.substring(input.indexOf("(") + 1, input.indexOf(")")).split(",").toList
    )

  def interpretInstruction(instruction: Instruction): Int =
    instruction.operation match
      case Operation.MUL => instruction.operands.map(_.toInt).reduce(_ * _)
      case Operation.NOP => 0

  def extactAllInstructions(input: String): List[Instruction] =
    mulRegex.findAllIn(input).map(parseInstruction).toList

  override def solve(input: List[String]): Int =
    input.flatMap(extactAllInstructions).map(interpretInstruction).sum
