package ch10

import ch10.Monoid.{dual, endoMonoid}
import ch3.{Branch, Leaf, Tree}
import ch3.Tree.fold

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  /**
   * p.236 10.15
   */
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(Nil: List[A])(_ :: _)
}

/**
 * p.236 10.13
 */
object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = fold(as)(f)(mb.op)
}

/**
 * p.236 10.14
 */
object OptionFoldable extends Foldable[Option]{
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
}