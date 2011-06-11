import scala.ozma._
import ozma._

import digitallogic._

object Utils {
  /**
   * Create a full adder digital circuit
   * Input: x, y, z
   * Output: c, s
   * Relation: x + y + Z = (cs)_2
   */
  def fullAdder(x: Signal, y: Signal, z: Signal) = {
    val k = x && y
    val l = y && z
    val m = x && z
    val c = k || l || m
    val s = z ^^ x ^^ y

    (c, s)
  }

  /**
   * Create a latch digital circuit
   * Input: control, input
   * Output: output
   * Relation:
   *   if (control) output = output(t-1)
   *   else         output = input
   */
  def latch(control: Signal, input: Signal) = {
    val output: Signal
    val f = Gates.Delay(output)
    val x = f && control
    val z = !control
    val y = z && input
    output = x || y
    output
  }

  /**
   * Display a set of digital signal
   * Each given signal is a column on the standard output.
   * It begins with the header and then its values at each
   * point of time.
   * This method never returns, as it expects signals to
   * grow indefinitely.
   */
  def display(signals: (Char, Signal)*) {
    def loop(signals: List[Signal]) {
      val next = for (signal <- signals) yield {
        print(signal.head + " ")
        signal.tail
      }

      println()
      loop(next)
    }

    val sigs = signals toList

    for (header <- sigs.map(_._1))
      print(header + " ")

    println()
    println()

    loop(sigs.map(_._2))
  }
}
