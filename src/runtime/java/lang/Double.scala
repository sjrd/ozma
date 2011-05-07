package java.lang

class Double(val doubleValue: scala.Double) extends Number {
  def byteValue = doubleValue.toByte
  def shortValue = doubleValue.toShort
  def intValue = doubleValue.toInt
  def longValue = doubleValue.toLong
  def floatValue = doubleValue.toFloat

  @native override def toString: String = sys.error("stub")
}

object Double {
  def valueOf(doubleValue: scala.Double) = new Double(doubleValue)
}
