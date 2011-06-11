package digitallogic

class SignalOps(signal: Signal) {
  import Gates._

  def unary_!() = Not(signal)

  def && (right: Signal) = And(signal, right)
  def || (right: Signal) = Or(signal, right)
  def !&& (right: Signal) = Nand(signal, right)
  def !|| (right: Signal) = Nor(signal, right)
  def ^^ (right: Signal) = Xor(signal, right)
}
