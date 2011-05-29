package scala

package object ozma {
  @native def newUnbound[A]: A = sys.error("stub")

  @native def thread[A](stat: => A): A = sys.error("stub")

  @native def waitBound(value: Any): Unit = sys.error("stub")
  @native def waitQuiet(value: Any): Unit = sys.error("stub")
  @native def waitNeeded(value: Any): Unit = sys.error("stub")

  @native def byNeed[A](value: => A): A = sys.error("stub")
  @native def byNeedFuture[A](value: => A): A = sys.error("stub")

  @native def makeFailedValue[A](throwable: Throwable): A = sys.error("stub")

  @native def sleep(ms: Int): Unit = sys.error("stub")

  def whileLoop(cond: => Boolean)(body: => Unit) {
    if (cond) {
      body
      whileLoop(cond)(body)
    }
  }

  def doWhileLoop(body: => Unit)(cond: => Boolean) {
    body
    if (cond)
      doWhileLoop(body)(cond)
  }
}
