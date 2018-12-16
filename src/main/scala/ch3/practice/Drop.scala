package ch3.practice

import ch3.{Cons, List, Nil}

object Drop extends App {

  /**
    * p.46 3.4
    */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  println(drop(List(1,2,3,4,5),2))

  /**
    * p.46 3.5
    */
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  println(dropWhile(List(1,2,3,4,5), (x: Int) => x <= 3))
}
