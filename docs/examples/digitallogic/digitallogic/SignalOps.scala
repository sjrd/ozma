package digitallogic

class SignalOps(signal: Signal) {
  import Gates._

  def unary_!() = not(signal)

  def && (right: Signal) = and(signal, right)
  def || (right: Signal) = or(signal, right)
  def !&& (right: Signal) = nand(signal, right)
  def !|| (right: Signal) = nor(signal, right)
  def ^^ (right: Signal) = xor(signal, right)
  def !^^ (right: Signal) = xnor(signal, right)
}
