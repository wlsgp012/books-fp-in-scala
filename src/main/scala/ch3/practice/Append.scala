package ch3.practice

import ch3.List._
import ch3.{Cons, List, Nil}

object Append extends App {

  def append[A](list: List[A], target: A): List[A] = foldRight(list, List(target))((current, acc) => Cons(current, acc))

  def append2[A](list: List[A], target:List[A]): List[A] = foldRight(list, target)(Cons(_, _))

  println(append(List(1,2,3,4,5), 6))
  println(append2(List(1,2,3,4,5), List(6, 7)))

}
