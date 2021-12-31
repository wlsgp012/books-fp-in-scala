package old.ch3.practice

import old.ch3.List._
import old.ch3.{Cons, List, Nil}

/**
  * p.52 3.12
  */
object Reverse extends App{

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => foldLeft(t, Cons(h, Nil))( (acc,h) => Cons(h, acc))
  }
  println(reverse(List(1,2,3,4)))

}
