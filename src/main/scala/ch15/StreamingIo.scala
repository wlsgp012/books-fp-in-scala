package ch15

import ch11.Monad
import ch15.Process.lift


sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  /**
   * p.361 15.5
   */
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
    p2 match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h, this |> t)
      case Await(f) => this match {
        case Emit(h, t) => t |> f(Some(h))
        case Halt() => Halt() |> f(None)
        case Await(g) => Await((i: Option[I]) => g(i) |> p2)
      }
    }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

  def liftOne[I, O](f: I => O): Await[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  /**
   * p.360 15.1
   */
  def id[I]: Process[I, I] = lift(identity)

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] = Emit(head, tail)

  def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Await[I, O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def take[I](n: Int): Process[I, I] =
    if (n <= 0) Halt()
    else await(i => emit(i, take[I](n - 1)))

  def drop[I](n: Int): Process[I, I] =
    if (n <= 0) id
    else await(i => drop[I](n - 1))

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    await(i =>
      if (f(i)) emit(i, takeWhile(f))
      else Halt())

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    await(i =>
      if (f(i)) dropWhile(f)
      else emit(i, id))


  def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
    new Monad[({ type f[x] = Process[I,x]})#f] {
      def unit[O](o: => O): Process[I,O] = emit(o)
      def flatMap[O,O2](p: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] =
        p flatMap f
    }


  def main(args: Array[String]): Unit = {
    //    val p = liftOne((x: Int) => x * 2)
    //    val xs = p(Stream(1, 2, 3)).toList
    //    println(p)
    //    println(xs)
    //    val units = Stream.continually(())
    //    val ones = lift((_: Unit) => 1)(units)
    //    println(ones)
    val even = filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1, 2, 3, 4)).toList
    println(evens)
  }

}