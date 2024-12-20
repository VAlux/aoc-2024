import scala.util.chaining.*
import scala.annotation.tailrec

object d5p2 extends Solution[Int]:
  import d5p1.*

  def correctOrder(order: PrintOrder, directRules: RuleMapping, inverseRules: RuleMapping): PrintOrder =

    @tailrec
    def go(current: List[Int], index: Int = 0): List[Int] =
      if index >= current.size then current
      else
        val number                   = order.pages(index)
        val directRule               = directRules.get(number)
        val inverseRule              = inverseRules.get(number)
        val (before, after)          = order.pages.splitAt(order.pages.indexOf(number))
        val (moveAfter, leaveBefore) =
          directRule.map(rule => before.partition(rule.next.contains)).getOrElse((List.empty, before))

        val (moveBefore, leaveAfter) =
          inverseRule.map(rule => after.tail.partition(rule.next.contains)).getOrElse((List.empty, after))

        val nextBefore = leaveBefore ++ moveBefore
        val nextAfter  = moveAfter ++ leaveAfter
        val next       = if index > 0 then (nextBefore :+ number) ++ nextAfter else nextBefore ++ nextAfter
        println(s"STEP $index")
        println(s"SPAN AT: ${order.pages(index)}")
        println(s"BEFORE $before AFTER: $after")
        println(s"NEXT BEFORE $nextBefore NEXT AFTER: $nextAfter")
        println(s"RES: $next")
        println("=" * 20)
        go(next, index + 1)

    val res = go(order.pages)
    PrintOrder(res)

  override def solve(input: List[String]): Int =
    val (rules, orders) = input.splitAt(input.indexOf(""))
    val parsedOrders    = parseOrders(rules)
    val directRules     = parsedOrders.pipe(groupOrdersToRules(_, false))
    val inverseRules    = parsedOrders.map(_.swap()).pipe(groupOrdersToRules(_, true))

    val invalidPrintOrders = parsePrints(orders.tail)
      .filterNot(order => isValidPrintOrder(order, directRules, inverseRules))

    println(invalidPrintOrders)

    println(s"INVALID ORDER: ${invalidPrintOrders(2)}")
    println(s"CORRECTED ORDER: ${correctOrder(invalidPrintOrders(2), directRules, inverseRules)}")

    // val correctedOrders = invalidPrintOrders.map(correctOrder(_, directRules, inverseRules))
    // println(correctedOrders)
    // correctedOrders.map(_.middle()).sum
    0
