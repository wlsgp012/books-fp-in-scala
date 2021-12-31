package old.ch3.practice

import old.ch3.List._
import old.ch3.{Cons, List, Nil}

object DoubleToString extends App {

  def convert(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((cur, acc) => Cons(cur.toString, acc))

  println(convert(List(1.1, 2.2, 3.3, 4.4)))

}
