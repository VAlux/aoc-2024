import scala.util.chaining.*
import d3p1.*

object d3p2 extends Solution[Int]:
  def interpret(instructions: List[Instruction], enabled: Boolean = true, acc: Int = 0): Int =
    if instructions.isEmpty then acc
    else
      val instruction = instructions.head
      instruction.operation match
        case Operation.MUL  =>
          if enabled then
            val result = instruction.operands.map(_.toInt).reduce(_ * _)
            interpret(instructions.tail, enabled, acc + result)
          else interpret(instructions.tail, enabled, acc)
        case Operation.DO   => interpret(instructions.tail, true, acc)
        case Operation.DONT => interpret(instructions.tail, false, acc)
        case Operation.NOP  => interpret(instructions.tail, enabled, acc)

  override def solve(input: List[String]): Int =
    input.flatMap(extactAllInstructions).pipe(interpret(_))
