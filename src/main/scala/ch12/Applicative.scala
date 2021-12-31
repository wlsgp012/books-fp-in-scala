package ch12

import ch10.{Foldable, Monoid}
import ch11.{Functor, Monad}
import ch6.State
import ch6.State._

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  /**
   * p.267 12.2
   */
  def apply_[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ab, a) => ab(a)) // map2(fab, fa)(_(_))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def map_[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](l: List[A])(f: A => F[B]): F[List[B]] = l.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /**
   * p.267 12.1
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) => a -> b)

  /**
   * p.268 12.3
   */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  /**
   * p.281 12.8
   */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    }
  }

  /**
   * p.282 12.12
   */
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map.empty[K, V])) {
    case (map, (k, fv)) => map2(map, fv)((m, v) => m + (k -> v))
  }

}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    override def apply[A, B](fab: Stream[A => B])(fa: Stream[A]): Stream[B] = map2(fab, fa)(_ (_))

    override def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = fa zip fb map f.tupled

  }

  /**
   * p.274 12.5
   */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Right(x) => f(x)
      case Left(e) => Left(e)
    }
  }

}

/**
 * p.276 12.6
 */
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = map2(fab, fa)(_ (_))

    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (_, Failure(h, t)) => Failure(h, t)
      case (Failure(h, t), _) => Failure(h, t)
    }
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

  def sequence_[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(ga => ga)

  /**
   * p.28 12.14
   */
  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idMonad)
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def apply[A, B](fab: Const[M, A => B])(fa: Const[M, A]): Const[M, B] = M.op(fab, fa)

    override def unit[A](a: => A): Const[M, A] = M.zero

    override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = M.op(fa, fb)
  }

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)


  def zipWithIndex[A](ta: F[A]): F[(A, Int)] = {
    val value: State[Int, F[(A, Int)]] = traverseS(ta)((a: A) => get[Int].flatMap(i => set(i + 1).map(_ => (a, i))))
    value.run(0)._1
  }

  override def toList[A](fa: F[A]): List[A] = traverseS(fa)((a: A) => {
    //    val value: State[List[A], Unit] = get[List[A]].flatMap(l => set(a :: l).map(_ => ()))
    //    value
    for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()
  }).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => {
      //      val x: State[S, B] = get[S].flatMap(s1 => {
      //        val (b, s2)=f(a, s1)
      //        set(s2).map(_ => b)
      //      })
      //      x
      for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b
    }).run(s)

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] = mapAccum(ta, 0)((a, s) => ((a, s), s + 1))._1

  def toList_[A](fa: F[A]): List[A] = toReveredList_(fa).reverse

  def toReveredList_[A](fa: F[A]): List[A] = mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2

  /**
   * p.288 12.16
   */
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toReveredList_(fa))((_, s) => (s.head, s.tail))._1

  /**
   * p.289 12.17
   */
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  /**
   * p.290 12.18
   */
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  /**
   * p.291 12.19
   */
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

  /**
   * p.291 12.20
   */
  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] =
    new Monad[({type f[x] = G[H[x]]})#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  /**
   * p.283 12.13
   */
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(b => Some(b))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}