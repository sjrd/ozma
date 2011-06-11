import scala.ozma._

package object digitallogic {
  type Signal = Signal.Signal

  implicit def int2bit(i: Int) = if (i == 0) Zero else One

  implicit def signal2ops(signal: Signal) = new SignalOps(signal)

  implicit def intList2signal(list: List[Int]): Signal =
    list.toAgent map int2bit
}
