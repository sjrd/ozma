import scala.ozma._

package object digitallogic {
  type Signal = List[Bit]

  implicit def int2bit(i: Int) = if (i == 0) Zero else One

  implicit def signal2ops(signal: Signal) = new SignalOps(signal)

  implicit def bit2builder(bit: Bit) = new SignalBuilder ++ bit
  implicit def int2builder(i: Int) = bit2builder(int2bit(i))

  implicit def builder2signal(builder: SignalBuilder) = builder.signal
}
