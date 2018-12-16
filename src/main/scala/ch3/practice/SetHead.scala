package ch3.practice

import ch3.{Cons, List, Nil}

/**
  * p.45 3.3
  */
object SetHead extends App {

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(_, tail) => Cons(head, tail)
  }

  println(setHead(List(1, 2, 3, 4, 5), 0))
  println(setHead(Nil, 0))
}
