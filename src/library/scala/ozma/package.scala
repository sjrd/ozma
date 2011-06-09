package scala

package object ozma {
  @native def newUnbound[@specialized A]: A = sys.error("stub")

  @native def thread[@specialized A](stat: => A): A = sys.error("stub")

  def threadGC[@specialized A, B >: Null](arg: B)(stat: B => A): A = {
    var argGC = arg
    thread {
      val arg = argGC
      argGC = null
      stat(arg)
    }
  }

  def threadGC[@specialized A, B >: Null, C >: Null](arg1: B, arg2: C)(
      stat: (B, C) => A): A = {
    var argGC1 = arg1
    var argGC2 = arg2
    thread {
      val arg1 = argGC1
      val arg2 = argGC2
      argGC1 = null
      argGC2 = null
      stat(arg1, arg2)
    }
  }

  @native def waitBound[@specialized A](value: A): Unit = sys.error("stub")
  @native def waitQuiet[@specialized A](value: A): Unit = sys.error("stub")
  @native def waitNeeded[@specialized A](value: A): Unit = sys.error("stub")

  @native def byNeed[@specialized A](value: => A): A = sys.error("stub")
  @native def byNeedFuture[@specialized A](value: => A): A = sys.error("stub")

  @native def makeFailedValue[@specialized A](throwable: Throwable): A =
    sys.error("stub")

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

  implicit def listWrapperToAgent[A](list: List[A]) =
    new ListWrapperToAgent(list)

  implicit def listUnwrapperFromAgent[A](agent: ListAgent[A]) = agent.toList

  class ListWrapperToAgent[A](list: List[A]) {
    def toAgent = new ListAgent(list)
  }
}
