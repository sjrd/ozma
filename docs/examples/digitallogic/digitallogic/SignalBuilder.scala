package digitallogic

import scala.ozma._
import ozma._

class SignalBuilder {
  private val (_signal, port) = Port.newPort[Bit]

  val signal: Signal = _signal

  def ++ (bit: Bit): this.type = {
    port.send(bit)
    this
  }

  def toSignal = signal
}
