package ch3.practice

import ch3.{Cons, List, Nil}

/**
  * p.47 3.6
  */
object Init extends App{

  def init[A](l: List[A]): List[A] = {
    def loop(m: List[A], n: List[A]):List[A] = m match {
      case Cons(_, Nil) => n
      case Cons(h, t) => loop(t, List.append(n, Cons(h, Nil)))
    }
    loop(l, Nil)
  }

  println(init(List(1,2,3,4,5,6)))

}
