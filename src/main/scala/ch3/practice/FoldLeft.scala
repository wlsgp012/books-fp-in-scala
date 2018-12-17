package ch3.practice

import ch3.{Cons, List, Nil}

object FoldLeft extends App {

  /**
    * p.51 3.10
    */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * p.52 3.11
    */
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

}
