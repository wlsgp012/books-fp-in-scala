package ch13

import ch11.Monad
import ch7.Par
import ch7.Par.Par

import scala.io.StdIn.readLine

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  /**
   * p.315 13.1
   */
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma flatMap f
  }

  /**
   * p.315 13.2 compile error
   */
  //  @annotation.tailrec
  //  def runTrampoline[A](fa: Free[Function0,A]): A = fa match {
  //    case Return(a) => a
  //    case Suspend(r) => r()
  //    case FlatMap(x,f) => x match {
  //      case Return(a) => runTrampoline { f(a) }
  //      case Suspend(r) => runTrampoline { f(r()) }
  //      case FlatMap(a,g) => runTrampoline { a flatMap { a0 => g(a0) flatMap f } }
  //    }
  //  }

  /**
   * p.315 13.3 compile error
   */
  //  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
  //    case Return(a) => F.unit(a)
  //    case Suspend(r) => r
  //    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
  //    case _ => sys.error("Impossible, since `step` eliminates these cases")
  //  }
  //  @annotation.tailrec
  //  def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
  //    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
  //    case FlatMap(Return(x), f) => step(f(x))
  //    case _ => a
  //  }
  def step[F[_], A](a: Free[F, A]): Free[F, A] = ???


}

sealed trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = Par.lazyUnit(run)

  override def toThunk = () => run

  def run: Option[String] =
    try Some(readLine())
    catch {
      case e: Exception => None
    }
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))

  override def toThunk: () => Unit = () => println(line)
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console.")
    ln <- readLn
  } yield ln

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

//  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = Free.step(free) {
//    case Return(a) => G.unit(a)
//    case Suspend(r) => t(r)
//    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
//    case _ => sys.error("Impossible; `step` eliminates these cases")
//  }

}


//sealed trait TailRec[A] {
//  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
//
//  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
//
//}
//
//case class Return[A](a: A) extends TailRec[A]
//
//case class Suspend[A](resume: () => A) extends TailRec[A]
//
//case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
//
//object TailRec extends Monad[TailRec] {
//  override def unit[A](a: => A): TailRec[A] = Return(a)
//
//  override def flatMap[A, B](ma: TailRec[A])(f: A => TailRec[B]): TailRec[B] = ma flatMap f
//
//  def suspend[A](a: => TailRec[A]): TailRec[A] = Suspend(() => ()).flatMap { _ => a }
//
//  /*
//  @tailrec
//  def run[A](io: TailRec[A]): A = io match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(x, f) => x match {
//      case Return(a) => run(f(a))
//      case Suspend(r) => run(f(r()))
////      case FlatMap(y, g) => run((y flatMap g) flatMap f)
//      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//    }
//  }
//  */
//
//  def forever[A, B](a: TailRec[A]): TailRec[B] = {
//    lazy val t: TailRec[B] = forever(a)
//    a flatMap (_ => t)
//  }
//
//  def printLine(s: String): TailRec[Unit] = Suspend(() => println(s))
//
//  def main(args: Array[String]): Unit = {
//    val p = TailRec.forever(printLine("Still going..."))
//  }
//}

//sealed trait Async[A] {
//  def flatMap[B](f: A => Async[B]): Async[B] =
//    FlatMap(this, f)
//
//  def map[B](f: A => B): Async[B] =
//    flatMap(f andThen (Return(_)))
//}
//
//case class Return[A](a: A) extends Async[A]
//
//case class Suspend[A](resume: Par[A]) extends Async[A]
//
//case class FlatMap[A, B](sub: Async[A],
//                         k: A => Async[B]) extends Async[B]
//
//object Async{
//  @annotation.tailrec
//  def step[A](async: Async[A]): Async[A] = async match {
//    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g)) case FlatMap(Return(x), f) => step(f(x))
//    case _ => async
//  }
//  def run[A](async: Async[A]): Par[A] = step(async) match { case Return(a) => Par.unit(a)
//  case Suspend(r) => Par.flatMap(r)(a => run(a))
//  case FlatMap(x, f) => x match {
//    case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
//    case _ => sys.error("Impossible; `step` eliminates these cases") }
//  }
//}