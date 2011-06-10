package scala.ozma

import util.control.Breaks

/**
 * This class provides GC-friendly operations on lists.
 * <p>Calling a method of List does not allow the GC to garbage collect the
 * list while it is being traversed, which make it inappropriate for an
 * agent-like usage of lists.
 * <p>Prefixing the method call by `.toAgent` will provide a GC-friendly view
 * of the list, allowing for proper garbage collection and agent usage.
 * <p>An instance of `ListAgent` can be used <i>once</i>. You may not call two
 * methods of the same instance. This would throw an `IllegalStateException`.
 */
class ListAgent[A](@tailcall private var _list: List[A])
    extends TraversableOnce[A] {
  private def list = {
    val result = _list
    if (result eq null)
      throw new IllegalStateException("Already used ListAgent")
    _list = null
    result
  }

  private lazy val breaks = new Breaks
  import breaks.{ breakable, break }

  def isTraversableAgain = false

  def foreach[U](f: A => U) = ListAgent.foreach(list)(f)
  def isEmpty = _list.isEmpty
  def hasDefiniteSize = false

  def seq: ListAgent[A] = this

  // Implementation copied from TraversableLike
  def forall(p: A => Boolean): Boolean = {
    var result = true
    breakable {
      for (x <- this)
        if (!p(x)) { result = false; break }
    }
    result
  }

  // Implementation copied from TraversableLike
  def exists(p: A => Boolean): Boolean = {
    var result = false
    breakable {
      for (x <- this)
        if (p(x)) { result = true; break }
    }
    result
  }

  // Implementation copied from TraversableLike
  def find(p: A => Boolean): Option[A] = {
    var result: Option[A] = None
    breakable {
      for (x <- this)
        if (p(x)) { result = Some(x); break }
    }
    result
  }

  // Implementation copied from TraversableLike
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    breakable {
      for (x <- this) {
        if (i >= end) break
        xs(i) = x
        i += 1
      }
    }
  }

  override def toList = list
  def toTraversable: Traversable[A] = list
  def toIterator: Iterator[A] = toStream.iterator
  def toStream: Stream[A] = list.toStream
  override def toSeq = list

  override def foldLeft[B](z: B)(op: (B, A) => B) =
    ListAgent.foldLeft(list)(z)(op)

  def map[B](f: A => B) = ListAgent(ListAgent.map(list)(f))

  def filter(p: A => Boolean) = ListAgent(ListAgent.filter(list)(p))

  def filterNot(p: A => Boolean) = ListAgent(ListAgent.filterNot(list)(p))

  def drop(n: Int): List[A] = ListAgent(ListAgent.drop(list)(n))
}

object ListAgent {
  def apply[A](@tailcall list: List[A]) = new ListAgent(list)

  def foreach[A, U](list: List[A])(f: A => U) {
    if (!list.isEmpty) {
      f(list.head)
      foreach(list.tail)(f)
    }
  }

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    if (list.isEmpty) Nil
    else f(list.head) :: map(list.tail)(f)
  }

  def filter[A](list: List[A])(p: A => Boolean): List[A] = {
    if (list.isEmpty) Nil
    else if (p(list.head)) list.head :: filter(list.tail)(p)
    else filter(list.tail)(p)
  }

  def filterNot[A](list: List[A])(p: A => Boolean): List[A] = {
    if (list.isEmpty) Nil
    else if (!p(list.head)) list.head :: filterNot(list.tail)(p)
    else filterNot(list.tail)(p)
  }

  def foldLeft[A, B](list: List[A])(z: B)(op: (B, A) => B): B = {
    if (list.isEmpty) z
    else foldLeft(list.tail)(op(z, list.head))(op)
  }

  def drop[A](list: List[A])(n: Int): List[A] = {
    if (n <= 0 || list.isEmpty) list
    else drop(list.tail)(n-1)
  }
}
