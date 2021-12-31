package old.ch3.practice

import old.ch3.{Cons, List, Nil}

/**
  * p.45 3.2
  */
object Tail extends App {

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, x) => x
  }

  println(tail(List(1, 2, 3, 4, 5)))
  println(tail(Nil))
}