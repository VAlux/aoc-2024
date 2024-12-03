import scala.util.chaining.*
import scala.annotation.tailrec

object d3p1 extends Solution[Int]:
  val mulRegex = s"mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)".r

  enum Operation:
    case MUL
    case DO
    case DONT
    case NOP

  case class Instruction(operation: Operation, operands: List[String])
  object Instruction:
    val NOP = Instruction(Operation.NOP, List.empty)

  def parseInstruction(input: String): Instruction =
    input match
      case "do()"    => Instruction(Operation.DO, List.empty)
      case "don't()" => Instruction(Operation.DONT, List.empty)
      case _         =>
        val operands = input.substring(input.indexOf("(") + 1, input.indexOf(")")).split(",").toList
        Instruction(Operation.MUL, operands)

  @tailrec
  def interpret(instructions: List[Instruction], acc: Int = 0): Int =
    if instructions.isEmpty then acc
    else
      val instruction = instructions.head
      instruction.operation match
        case Operation.MUL =>
          val result = instruction.operands.map(_.toInt).reduce(_ * _)
          interpret(instructions.tail, acc + result)
        case _             => interpret(instructions.tail, acc)

  def extactAllInstructions(input: String): List[Instruction] =
    mulRegex.findAllIn(input).map(parseInstruction).toList

  override def solve(input: List[String]): Int =
    input.flatMap(extactAllInstructions).pipe(interpret(_))
