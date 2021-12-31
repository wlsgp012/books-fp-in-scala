package ch2

case object Composition extends App{
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def compose2[A,B,C](f: B => C, g: A => B): A => C = f compose g
}
