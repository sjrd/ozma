package digitallogic

import scala.ozma._

object Gates {
  def not(input: Signal): Signal =
    thread(input.toAgent map (~_))

  private def makeGate(combination: (Bit, Bit) => Bit)(
      left: Signal, right: Signal): Signal = {
    thread(left.toAgent.zipMap(right)(combination))
  }

  val and  = makeGate(_ & _) _
  val or   = makeGate(_ | _) _
  val nand = makeGate(_ ~& _) _
  val nor  = makeGate(_ ~| _) _
  val xor  = makeGate(_ ^ _) _
  def xnor = makeGate(_ ~^ _) _

  def delay(input: Signal) = Zero :: input

  def delay(input: Signal, count: Int): Signal =
    if (count <= 0) input
    else delay(delay(input), count-1)
}
