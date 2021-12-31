package old.ch3.practice

import old.ch3.List._
import old.ch3.{List, Nil}

object FlatMap extends App {

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  println(flatMap(List(1, 2, 3))(i => List(i, i)))

}
