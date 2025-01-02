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

        go(next, index + 1)

    val res = go(order.pages)
    PrintOrder(res)

  object TopologicalSorting:
    def getRoot(graph: RuleMapping): Option[Int] =
      val descendants = graph.values.flatMap(_.next).toSet
      graph.keySet.diff(descendants).headOption

    def sort(order: PrintOrder, root: Int, graph: RuleMapping): List[Int] =
      val orderItems = order.pages.toSet

      def bfs(current: Int, descendants: Set[Int], acc: List[Int] = List.empty): List[Int] =
        if descendants.isEmpty then acc
        else
          val newCurrent     = descendants.head
          val newDescendants = descendants.tail ++ graph.get(newCurrent).map(_.next).getOrElse(Set())
          bfs(newCurrent, newDescendants, acc ++ orderItems.intersect(descendants))

      val initial = if orderItems.contains(root) then List(root) else List.empty
      bfs(root, graph(root).next, initial)

  override def solve(input: List[String]): Int =
    val (rules, orders) = input.splitAt(input.indexOf(""))
    val parsedOrders    = parseOrders(rules)
    val directRules     = parsedOrders.pipe(groupOrdersToRules(_, false))
    val inverseRules    = parsedOrders.map(_.swap()).pipe(groupOrdersToRules(_, true))

    val invalidPrintOrders = parsePrints(orders.tail)
      .filterNot(order => isValidPrintOrder(order, directRules, inverseRules))

    val root = TopologicalSorting.getRoot(directRules)
    val res  = TopologicalSorting.sort(invalidPrintOrders(0), root.get, directRules)
    println(s"INVALID: ${invalidPrintOrders(0)} VALID: ${res}")
    0
