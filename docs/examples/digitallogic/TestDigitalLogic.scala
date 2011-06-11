import scala.ozma._
import ozma._

import digitallogic._

import Utils._

object TestDigitalLogic {
  def main(args: Array[String]) {
    testFullAdder()
    testLatch()
    testClock()
  }

  /* Full adder */

  def testFullAdder() {
    println("Full adder")
    println()

    val x = Signal(1, 1, 0)
    val y = Signal(0, 1, 0)
    val z = Signal(1, 1, 1)

    val (c, s) = fullAdder(x, y, z)

    assert(c == Signal(1, 1, 0))
    assert(s == Signal(0, 1, 1))

    display('x' -> x, 'y' -> y, 'z' -> z,
            'c' -> c, 's' -> s)

    println()
  }

  def fullAdder(x: Signal, y: Signal, z: Signal) = {
    val t = x ^^ y
    val c = (t && z) || (x && y)
    val s = t ^^ z

    (c, s)
  }

  /* Latch */

  def testLatch() {
    println("Latch")
    println()

    val control = Signal(0, 0, 1, 1, 0, 0, 1)
    val input   = Signal(1, 0, 0, 1, 1, 0, 1)

    val output = latch(control, input)

    assert(output == Signal(1, 0, 0, 0, 1, 0, 0))

    display('c' -> control,
            'i' -> input,
            'o' -> output)

    println()
  }

  def latch(control: Signal, input: Signal) = {
    val output: Signal
    val f = Gates.delay(output)
    val x = f && control
    val z = !control
    val y = z && input
    output = x || y
    output
  }

  /* Clock */

  def testClock() {
    println("Clock")
    println()

    val clock = Signal.clock()
    val left = Signal.cycle(clock, 1, 0, 1)
    val right = Signal.cycle(clock, 0, 1)
    val output = left && right

    display('l' -> left, 'r' -> right, 'o' -> output)
  }
}
