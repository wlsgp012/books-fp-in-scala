package ch2.practice

/**
  * p.35 2.6
  */
object Compose extends App {

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  def compose2[A, B, C](f: B => C, g: A => B): A => C = a => f.compose(g)(a)
}
