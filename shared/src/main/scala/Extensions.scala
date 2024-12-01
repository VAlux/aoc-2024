import scala.annotation.tailrec
object Extensions:

  extension [A](set: Set[A])
    def containsAny(other: Set[A]): Boolean =
      set.exists(other.contains)

    def containsExactly(other: Set[A], amount: Int): Boolean =
      set.count(other.contains) == amount

  extension [A](list: List[A])
    def tailOrEmpty: List[A] =
      if list.isEmpty then List.empty else list.tail

    def segment(predicate: A => Boolean): List[List[A]] =
      @tailrec
      def go(current: A, rem: List[A], acc: List[List[A]] = List.empty): List[List[A]] =
        if rem.isEmpty then acc
        else if predicate(current) then go(rem.head, rem.tail, acc :+ List())
        else go(rem.head, rem.tail, acc.updated(acc.size - 1, acc.last :+ current))

      list match
        case first :: rest => go(rest.head, rest.tail, List(List(first)))
        case _             => List.empty

  extension (str: String)
    def isBlank: Boolean =
      str.trim.isEmpty
