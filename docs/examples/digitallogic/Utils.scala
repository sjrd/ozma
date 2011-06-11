import scala.ozma._
import ozma._

import digitallogic._

object Utils {
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
        if (signal.isEmpty)
          return

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
