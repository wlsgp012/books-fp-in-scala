package ch2

import scala.annotation.tailrec

object IsSorted extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    //    @tailrec
    //    def go(n: Int, before: A): Boolean = {
    //      if(n >= as.length) true
    //      else if (!ordered(before, as(n))) false
    //      else go(n+1, as(n))
    //    }
    //    go(0, as(0))
    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }
}
