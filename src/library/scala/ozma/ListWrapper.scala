package scala.ozma

class ListWrapper[A](list: List[A]) {
  def toAgent = new ListAgent(list)

  def lazified = new LazyList(list)
}
