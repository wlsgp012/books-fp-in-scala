package old.ch2.practice

/**
  * p.27 2.1
 */
object Fibo extends App {

  def fib(n: Long): Long = {
    if (n == 0 || n == 1) n
    else fib(n - 1) + fib(n - 2)
  }

  def fibTail(n: Long): Long = {
    @annotation.tailrec
    def go(m: Long, before: Long, next: Long): Long = {
      if (m == 0) before
      else go(m - 1, next, before + next)
    }
    go(n, 0, 1)
  }

  println(fibTail(45))
  println(fib(45))

}
