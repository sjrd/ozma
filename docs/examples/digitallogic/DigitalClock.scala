import scala.ozma._
import ozma._

import digitallogic._

import Utils._

object DigitalClock {
  def main(args: Array[String]) {
    val count = args.headOption map (_.toInt) getOrElse 8
    val clock = Signal.clock()
    val zeros = Signal.generator(clock)(i => Zero)

    def makeOutputs(i: Int, carry: Signal): List[Signal] = {
      if (i == count) Nil
      else {
        val prev: Signal
        val result = prev ^^ carry
        val nextCarry = prev && carry
        prev = Gates.delay(result)
        result :: makeOutputs(i+1, nextCarry)
      }
    }

    val outputs = makeOutputs(0, clock) reverse

    val params = outputs map (' ' -> _)
    display(params:_*)
  }
}
