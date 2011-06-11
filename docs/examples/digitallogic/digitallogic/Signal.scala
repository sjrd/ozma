package digitallogic

import scala.ozma._

object Signal {
  type Signal = List[Bit]

  def apply(bits: Bit*): Signal = List(bits:_*)
  def unapplySeq(signal: Signal): Some[List[Bit]] = Some(signal)

  def clock(delay: Int = 1000, value: Bit = One) = {
    def loop(): Signal = {
      sleep(delay)
      value :: loop()
    }

    thread(loop())
  }

  def generator(clock: Signal)(value: Int => Bit) = {
    def loop(clock: Signal, i: Int): Signal = {
      waitBound(clock)
      value(i) :: loop(clock.tail, i+1)
    }

    thread(loop(clock, 0))
  }

  def cycle(clock: Signal, values: Bit*) = {
    generator(clock) {
      i => values(i % values.length)
    }
  }
}
