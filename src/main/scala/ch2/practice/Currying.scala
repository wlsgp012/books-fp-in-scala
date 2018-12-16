package ch2.practice

/**
  * p.34,35 2.3~2.4
  */
object Currying extends App {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def partial[A, B, C](a: A, f:(A,B) => C): B => C = b => f(a, b)
}
