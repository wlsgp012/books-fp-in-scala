package old.ch3.practice

import old.ch3.List._
import old.ch3.{Cons, List, Nil}

object Filter extends App {

  def filter[A](as:List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)

  def filter2[A](as:List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if(f(h)) buf += h; go(t)
    }
    go(as)
    List(buf.toList: _*)
  }

  val x = List(1,2,3,4,5,6,7,8)
  val f = (x: Int) => x % 2 == 0
  println(filter(x)(f))
  println(filter2(x)(f))

}
