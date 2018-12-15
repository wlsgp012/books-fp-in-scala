package ch2.practice

/**
  * p.31 2.2
  */
object Sort extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    // my solution. It is poor
    /*
    @annotation.tailrec
    def loop(index: Int, before: A): Boolean = {
      if (index >= as.length) true
      else if (!ordered(before, as(index))) false
      else loop(index + 1, as(index))
    }

    loop(1, as(0))
    */
    // answer
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }

  println(isSorted(Array(1, 2, 3, 4, 5, 10), (a: Int, b: Int) => a <= b))
}
