package old.ch3.practice

import old.ch3.List._
import old.ch3.{Cons, List, Nil}

object PlusOne extends App {

  def plusOne(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((curr,acc) => Cons(curr+1, acc))

}
