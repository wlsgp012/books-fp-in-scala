package ch2

// 이것은 주석!
/* 이것도 주석 */
/** 문서화 주석 */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int ={
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <=0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }


  private def formatResult(name:String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
