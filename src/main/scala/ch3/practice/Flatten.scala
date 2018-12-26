package ch3.practice

import ch3.List._
import ch3.{List, Nil}

object Flatten extends App {

  def flatten[A](list: List[List[A]]): List[A] = foldRight(list, Nil: List[A])(append)

  val x = List(List(1, 2, 3), List(4, 5))

  println(flatten(x))
}
