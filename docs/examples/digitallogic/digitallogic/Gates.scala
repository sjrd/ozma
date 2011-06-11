package digitallogic

import scala.ozma._

object Gates {
  def Not(input: Signal): Signal = {
    def loop(input: Signal): Signal =
      ~input.head :: loop(input.tail)

    thread(loop(input))
  }

  private def makeGate(
      combination: (Bit, Bit) => Bit): (Signal, Signal) => Signal = {
    def loop(left: Signal, right: Signal): Signal = {
      combination(left.head, right.head) :: loop(left.tail, right.tail)
    }

    (leftInput, rightInput) => thread(loop(leftInput, rightInput))
  }

  val And  = makeGate(_ & _)
  val Or   = makeGate(_ | _)
  val Nand = makeGate(_ ~& _)
  val Nor  = makeGate(_ ~| _)
  val Xor  = makeGate(_ ^ _)

  def Delay(input: Signal) = Zero :: input

  def Clock(delay: Int = 1000, value: Bit = One) = {
    def loop(): Signal = {
      sleep(delay)
      value :: loop()
    }

    thread(loop())
  }

  def Generator(clock: Signal)(value: Int => Bit) = {
    def loop(clock: Signal, i: Int): Signal = {
      waitBound(clock)
      value(i) :: loop(clock.tail, i+1)
    }

    thread(loop(clock, 0))
  }

  def Cycle(clock: Signal, values: Bit*) = {
    Generator(clock) {
      i => values(i % values.length)
    }
  }
}
