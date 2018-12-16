package ch3.practice

import ch3.{Cons, List, Nil}
import ch3.List._

object FoldRight extends App {

  /**
    * p.51 3.8
    */
  val result = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  println(result)

  /**
    * p.51 3.9
    */
  def length[A](as: List[A]): Int = foldRight(as, 0)( (_, acc) => 1 + acc)

  def length2[A](as: List[A]): Int = as match {
    case Nil => 0
    case Cons(_, t) => 1 + length2(t)
  }

  println(length(List(1,2,3,4)))
  println(length2(List(1,2,3,4)))

}
