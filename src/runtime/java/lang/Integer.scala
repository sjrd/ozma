package java.lang

class Integer(val intValue: Int) extends Number {
  def byteValue = intValue.toByte
  def shortValue = intValue.toShort
  def longValue = intValue.toLong
  def floatValue = intValue.toFloat
  def doubleValue = intValue.toDouble

  @native override def toString: String = sys.error("stub")
}

object Integer {
  def valueOf(intValue: Int) = new Integer(intValue)
}
