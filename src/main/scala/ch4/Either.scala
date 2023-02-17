package ch4

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  /**
   * p.77 4.6
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }

  //  def map2[EE >: E, B, C](b: Either [EE, B])(f: (A,B) => C): Either[EE, C] = this flatMap(aa => b.map(bb => f(aa, bb)))
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)

  /**
   * p.77 4.7
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => (f(h) map2 (traverse(t)(f))) (_ :: _)
    //    case h :: t => for {x <- f(h); y <- traverse(t)(f)} yield (x :: y)
  }

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
}
