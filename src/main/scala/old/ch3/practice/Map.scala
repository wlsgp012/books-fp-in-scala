package old.ch3.practice

import old.ch3.List._
import old.ch3.{Cons, List, Nil}

object Map extends App {

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])( (h, t) => Cons(f(h), t))

  val x = List(1,2,3,4,5)

  val f = (x: Int) => x + 1

  println(map(x)(f))

}
