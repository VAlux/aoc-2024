import scala.util.chaining.*

object d5p1 extends Solution[Int]:
  type RuleMapping = Map[Int, Rule]

  case class Order(left: Int, right: Int):
    def swap(): Order = Order(right, left)

  case class Rule(start: Int, next: Set[Int], inverse: Boolean)
  case class PrintOrder(pages: List[Int]):
    def middle(): Int = pages(pages.size / 2)

  def parseOrders(input: List[String]): List[Order] =
    input
      .map(elem => elem.split("\\|"))
      .map:
        case Array(l, r) => Order(l.toInt, r.toInt)

  def groupOrdersToRules(orders: List[Order], inverse: Boolean): RuleMapping =
    orders
      .groupBy(_.left)
      .map { case (start, orders) => start -> Rule(start, orders.map(_.right).toSet, inverse) }

  def parsePrints(input: List[String]): List[PrintOrder] =
    input.map(_.split(",").map(_.toInt).toList).map(PrintOrder.apply)

  def isValidPrintOrder(order: PrintOrder, directRules: RuleMapping, inverseRules: RuleMapping): Boolean =
    def isPageValid(page: List[Int], rule: Rule, inverse: Boolean): Boolean =
      if inverse then !page.exists(rule.next.contains) else page.forall(rule.next.contains)

    def checkRules(rules: RuleMapping, current: Int, next: List[Int], inverse: Boolean): Boolean =
      rules
        .get(current)
        .map(rule => isPageValid(next, rule, inverse))
        .getOrElse(true)

    def check(currentPage: Int, nextPages: List[Int], res: Boolean = true): Boolean =
      if nextPages.isEmpty || !res then res
      else
        val valid = checkRules(directRules, currentPage, nextPages, false)
          && checkRules(inverseRules, currentPage, nextPages, true)

        check(nextPages.head, nextPages.tail, valid)

    check(order.pages.head, order.pages.tail)

  override def solve(input: List[String]): Int =
    val (rules, orders) = input.splitAt(input.indexOf(""))
    val parsedOrders    = parseOrders(rules)
    val directRules     = parsedOrders.pipe(groupOrdersToRules(_, false))
    val inverseRules    = parsedOrders.map(_.swap()).pipe(groupOrdersToRules(_, true))

    parsePrints(orders.tail)
      .filter(order => isValidPrintOrder(order, directRules, inverseRules))
      .map(_.middle())
      .sum
