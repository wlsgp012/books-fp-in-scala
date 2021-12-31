package ch9

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  implicit def regex(r: Regex): Parser[String]

  /**
   * p.202 9.8
   */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A] // = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  /**
   * p.202 9.7
   */
  // my answer
  //  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p.flatMap(a => p2.flatMap(b => succeed(a -> b)))
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p.flatMap(a => p2.map(b => (a, b)))

  /**
   * p.199 9.4
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _) else succeed(List())

  /**
   * p.198 9.1
   */
  //  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(a => f(a._1, a._2))
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(f.tupled)
  /**
   * p.202 9.7
   */
  // my answer
//  def map2_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = p.flatMap(a => p2.map(b => f(a,b)))
  def map2_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for { a <- p; b <- p2 } yield f(a, b)



  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }
}
