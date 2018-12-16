package ch3.practice

import ch3.{Cons, List, Nil}

object FoldLeft extends App {

  /**
    * p.51 3.9
    */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

}
