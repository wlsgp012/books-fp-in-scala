package ch8

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.util.Random

object ScalaCheck extends App {
  val intList = Gen.listOf(Gen.choose(0, 100))
  val prop = forAll(intList)(ns => ns.reverse.reverse == ns) && forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
  val failingProp = forAll(intList)(ns => ns.reverse == ns)

  println(prop.check)
  println("===========")
  println(failingProp.check)
  println("===========")

  /**
   * p.159 8.1
   */
  val sum: List[Int] => Int = l => l.sum
  val proofOfSum =
    forAll(intList)(l => sum(l.reverse) == sum(l)) &&
      forAll(intList)(l => sum(l) == sum(Random.shuffle(l))) &&
      forAll(intList)(l => {
        val (f, s) = l.splitAt(l.size / 2)
        sum(f) + sum(s) == sum(l)
      })
  println(proofOfSum.check)

}
