package java.lang

class Float(val floatValue: scala.Float) extends Number {
  def byteValue = floatValue.toByte
  def shortValue = floatValue.toShort
  def intValue = floatValue.toInt
  def longValue = floatValue.toLong
  def doubleValue = floatValue.toDouble

  @native override def toString: String = sys.error("stub")
}

object Float {
  def valueOf(floatValue: scala.Float) = new Float(floatValue)
}
