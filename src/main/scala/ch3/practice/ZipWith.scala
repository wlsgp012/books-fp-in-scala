package ch3.practice

import ch3.{Cons, List, Nil}

object ZipWith extends App {

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  val a = List(1, 2, 3)
  val b = List(4, 5, 6)

  println(zipWith(a, b)((a, b) => a + b))
  println(zipWith(a, b)(_ + _))

}
