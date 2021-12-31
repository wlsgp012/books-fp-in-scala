package ch7

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  /**
   * p.125 7.1
   */
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val parC: Par[C => D] = map2(pa, pb)((a, b) => (c: C) => f(a, b, c))
    map2(parC, pc)((fc, c) => fc(c))
  }

  /**
   * p.134 7.4
   */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  //  def sortPar(parList: Par[List[Int]]): Par[List[Int]]= map2(parList, unit())((a, _) => a.sorted)
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit())((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
   * p.137 7.5
   */
  //  def sequence_[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List()): Par[List[A]])((pa, pb) => map2(pa, pb)((a, value) => a :: value))
  def sequence_[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List()): Par[List[A]])(map2(_, _)(_ :: _))

  // from the answer
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

  /**
   * p.137 7.6
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // this is wrong! It used single thread
    // fork { as.foldRight(unit(List()): Par[List[A]])((a, pb) => map2(unit(a), pb)((aa, xs) => if (f(aa)) aa :: xs else xs)) }

    // this is right.
    val pars: List[Par[List[A]]] = as.map(a => map(lazyUnit(List(a)))(_.filter(f)))
    map(sequence(pars))(xs => xs.flatMap(identity))

    // It's the answer on github
    //    val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    //    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => if (run(es)(cond).get) t(es) else f(es)

  /**
   * p.152 7.11
   */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val idx = run(es)(n).get
    run(es)(choices(idx))
  }

  def choice_[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  /**
   * p.153 7.12
   */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val k = run(es)(key).get
    run(es)(choices(k))
  }

  /**
   * p.153 7.13
   */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get
    run(es)(choices(a))
  }

  def choice__[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(b => if (b) t else f)

  def choiceMap_[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = chooser(key)(k => choices(k))

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = chooser(pa)(choices)

  /**
   * p.153 7.14
   */
  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(i => i)

  def flatMap_[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = join(map(pa)(a => choices(a)))

  def join_[A](a: Par[Par[A]]): Par[A] = es => {
    val pa = run(es)(a).get()
    run(es)(pa)
  }

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  def main(args: Array[String]): Unit = {
//    val a= lazyUnit(42 +1)
//    val s= Executors.newFixedThreadPool(1)
//    println(Par.equal(s)(a, fork(a)))

    val start = System.currentTimeMillis
    val xs = (1 to 1000).toList
    //    xs.filter(isEven)
    val es = Executors.newWorkStealingPool()
    testParFilter(xs, es)
    //    testParMap(xs, es)

    println(s"Finished! It took ${System.currentTimeMillis - start} milliseconds")
  }

  val isEven = (a: Int) => {
    TimeUnit.MILLISECONDS.sleep(1)
    println(Thread.currentThread().getName(), a)
    a % 2 == 0
  }

  def testParFilter(xs: List[Int], es: ExecutorService) {
    val value = parFilter(xs)(isEven)
    val value1 = run(es)(value)
    println(value1.get())
  }

  def testParMap(xs: List[Int], es: ExecutorService) {
    val multiplyTen = (x: Int) => {
      println(Thread.currentThread().getName(), x)
      x * 10
    }
    val par = parMap(xs)(multiplyTen)
    val v = run(es)(par)
    println(v.get())
  }
}