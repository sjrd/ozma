/**
 * This program must be given an integer N as command line parameter.
 * It will display the N first prime numbers.
 * It computes them using the sieve of Eratosthenes, with lazy functions
 * working with lists.
 * Note that all this does work because :: is tail-recursive.
 */

import scala.ozma._

object PrimeNumbers {
  def main(args: Array[String]) {
    val count = args(0).toInt
    val result = sieve(generateFrom(2))
    (result.lazified take count) foreach println
  }

  def generateFrom(from: Int): List[Int] = byNeedFuture {
    from :: generateFrom(from + 1)
  }

  def sieve(list: List[Int]): List[Int] = byNeedFuture {
    list match {
      case Nil => Nil
      case head :: tail =>
        head :: sieve(tail.lazified filter (_ % head != 0))
    }
  }
}
