/**
 * This program must be given an integer N as command line parameter.
 * It will display the N first prime numbers.
 * It computes them using the sieve of Eratosthenes, with lazy functions
 * working with lists.
 * Note that all this does work because :: is tail-recursive.
 */

import ozma._
import scala.ozma._

object PrimeNumbers {
  def main(args: Array[String]) {
    val count = java.lang.Integer.parseInt(args(0))
    val result = sieve(generateFrom(2))
    thread(result take count) foreach println
  }

  def generateFrom(from: Int): List[Int] = byNeedFuture {
    from :: generateFrom(from + 1)
  }

  def sieve(list: List[Int]): List[Int] = byNeedFuture {
    list match {
      case Nil => Nil
      case head :: tail =>
        head :: sieve(lazyFilter(tail, _ % head != 0))
    }
  }

  def lazyFilter(list: List[Int],
      predicate: Int => Boolean): List[Int] = byNeedFuture {
    list match {
      case Nil => Nil
      case head :: tail =>
        if (predicate(head))
          head :: lazyFilter(tail, predicate)
        else
          lazyFilter(tail, predicate)
    }
  }
}
