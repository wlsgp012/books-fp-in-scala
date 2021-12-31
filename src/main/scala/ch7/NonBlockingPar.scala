package ch7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, TimeUnit}


sealed trait Future[A]{
  private[ch7] def apply(k: A => Unit): Unit
}

object NonBlockingPar {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown}
    latch.await
    ref.get
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call = r})

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit())((a, _) => f(a))

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => new Future[C] {
      override def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A,B]](es){
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))((value, value1) => value ++ value1)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))


  def main(args: Array[String]): Unit = {
    val p = parMap(List.range(1, 100000))(math.sqrt(_))
    val x = run(Executors.newFixedThreadPool(2))(p)
    println(x)
  }

}