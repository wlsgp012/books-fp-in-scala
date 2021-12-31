package ch11

import ch12.Applicative
import ch6.State
import ch7.Par
import ch7.Par.Par
import ch8.Gen

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}


trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]


  /**
   * p.267 12.2
   */
  override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  /**
   * p.255 11.8
   */
  //  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((a: F[A]) => a, f)(ma)
  def flatMap_[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  override def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  /**
   * p.256 11.12
   */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  /**
   * p.256 11.13
   */
  def flatMap__[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def compose_[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
   * p.250 11.3
   */
  override def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(Nil): F[List[A]])((fa, acc) => map2(fa, acc)(_ :: _))

  // my answer
  // def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la flatMap f)
  override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  /**
   * p.250 11.4
   */
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] = if (n > 0) map2(ma, replicateM(n - 1, ma))(_ :: _) else unit(List[A]())
  //  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  /**
   * p.251 11.6
   */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(bool => if (bool) map(filterM(t)(f))(h :: _) else filterM(t)(f))
  }

  /**
   * p.251 11.7
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  // compose(compose(f, g) h) == compose(f, compose(g, h))

  type IntState[String] = State[Int, String]

}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  /**
   * p.249 11.1
   */
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
  }
}

/**
 * p.258 11.17
 */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

/**
 * p.263 11.20
 */
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
  }
}