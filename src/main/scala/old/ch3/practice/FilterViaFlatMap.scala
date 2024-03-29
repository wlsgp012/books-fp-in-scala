package old.ch3.practice

import old.ch3.List._
import old.ch3.{List, Nil}

object FilterViaFlatMap extends App {

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if(f(a)) List(a) else Nil)
  }

}
