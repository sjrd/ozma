import scala.ozma._
import ozma._

import digitallogic._

import Utils._

object TestDigitalLogic {
  def main(args: Array[String]) {
    (args.headOption getOrElse "") match {
      case "adder" => testFullAdder()
      case "latch" => testLatch()
      case "clock" => testClock()
      case _ =>
        println("Specify one of:")
        println("  adder, latch, clock")
    }
  }

  /* Full adder */

  def testFullAdder() {
    val x = 1 ++ 1 ++ 0 toSignal
    val y = 0 ++ 1 ++ 0 toSignal
    val z = 1 ++ 1 ++ 1 toSignal

    val (c, s) = fullAdder(x, y, z)

    display('x' -> x, 'y' -> y, 'z' -> z,
            'c' -> c, 's' -> s)
  }

  /* Latch */

  def testLatch() {
    val control = 0 ++ 0 ++ 1 ++ 1 ++ 0 ++ 0 ++ 1 toSignal
    val input   = 1 ++ 0 ++ 0 ++ 1 ++ 1 ++ 0 ++ 1 toSignal

    val output = latch(control, input)

    display('c' -> control,
            'i' -> input,
            'o' -> output)
  }

  /* Clock */

  def testClock() {
    import Gates._

    val clock = Clock()
    val left = Cycle(clock, 1, 0, 1)
    val right = Cycle(clock, 0, 1)
    val output = left && right

    display('l' -> left, 'r' -> right, 'o' -> output)
  }
}
