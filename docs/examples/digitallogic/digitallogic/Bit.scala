package digitallogic

import scala.ozma._

sealed abstract class Bit(bool: Boolean) {
  val toBool = bool
  val toInt = if (bool) 1 else 0
  val name = toInt.toString

  override def toString() = name

  def unary_~() =
    if (this eq One) Zero else One

  def | (right: Bit) =
    if ((this eq One) || (right eq One)) One else Zero

  def & (right: Bit) =
    if ((this eq One) && (right eq One)) One else Zero

  def ^ (right: Bit) =
    if (this eq right) Zero else One

  def ~& (right: Bit) = ~(this & right)
  def ~| (right: Bit) = ~(this | right)
  def ~^ (right: Bit) = ~(this ^ right)
}

case object Zero extends Bit(false)
case object One extends Bit(true)
