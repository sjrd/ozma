package scala

object Console {
  @native def print(s: String): Unit = sys.error("stub")
  def print(any: Any): Unit = print(any.toString)

  @native def println(s: String): Unit = sys.error("stub")
  def println(any: Any): Unit = println(any.toString)
}
