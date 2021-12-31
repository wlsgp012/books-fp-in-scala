package ch2

import scala.annotation.tailrec

object Fibo2 extends App {

  def fib(n: Int): Int = {
    @tailrec
    def go(m: Int, now: Int, next: Int): Int = {
      if(m == n) now
      else go(m+1, next, now + next)
    }
    go(0,0, 1)
  }
  println(fib(0)) //0
  println(fib(1)) //1
  println(fib(2)) //1
  println(fib(3)) //2
  println(fib(4)) //3
  println(fib(5)) //5
  println(fib(6)) //8
  println(fib(45)) //1134903170
}
