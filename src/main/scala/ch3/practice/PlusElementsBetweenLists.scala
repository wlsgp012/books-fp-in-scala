package ch3.practice

import ch3.{Cons, List, Nil}

object PlusElementsBetweenLists extends App {

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  val a = List(1, 2, 3)
  val b = List(4, 5, 6)

  println(addPairwise(a, b))
}
