package scala.ozma

/**
 * Lazified view of a list
 * <p>The methods of this class are lazy versions of the corresponding methods
 * in List.
 */
class LazyList[A](@tailcall private val list: List[A]) {
  def toList = list

  def map[B](f: A => B) = LazyList(LazyList.map(list)(f))

  def filter(p: A => Boolean) = LazyList(LazyList.filter(list)(p))

  def filterNot(p: A => Boolean) = LazyList(LazyList.filterNot(list)(p))

  def take(n: Int) = LazyList(LazyList.take(list)(n))

  def drop(n: Int) = LazyList(LazyList.drop(list)(n))

  def zip[B](that: List[B]) = LazyList.zip(list)(that)

  def zipMap[B, C](that: List[B])(f: (A, B) => C) =
    ListAgent.zipMap(list)(that)(f)
}

object LazyList {
  def apply[A](@tailcall list: List[A]) = new LazyList(list)

  def map[A, B](list: List[A])(f: A => B): List[B] = byNeedFuture {
    if (list.isEmpty) Nil
    else f(list.head) :: map(list.tail)(f)
  }

  def filter[A](list: List[A])(p: A => Boolean): List[A] = byNeedFuture {
    if (list.isEmpty) Nil
    else if (p(list.head)) list.head :: filter(list.tail)(p)
    else filter(list.tail)(p)
  }

  def filterNot[A](list: List[A])(p: A => Boolean): List[A] = byNeedFuture {
    if (list.isEmpty) Nil
    else if (!p(list.head)) list.head :: filterNot(list.tail)(p)
    else filterNot(list.tail)(p)
  }

  def take[A](list: List[A])(n: Int): List[A] = byNeedFuture {
    if (n <= 0 || list.isEmpty) Nil
    else list.head :: take(list.tail)(n-1)
  }

  def drop[A](list: List[A])(n: Int): List[A] = byNeedFuture {
    if (n <= 0 || list.isEmpty) list
    else drop(list.tail)(n-1)
  }

  def zip[A, B](list: List[A])(that: List[B]): List[(A, B)] = byNeedFuture {
    if (list.isEmpty || that.isEmpty) Nil
    else (list.head, that.head) :: zip(list.tail)(that.tail)
  }

  def zipMap[A, B, C](list: List[A])(that: List[B])(
      f: (A, B) => C): List[C] = byNeedFuture {
    if (list.isEmpty || that.isEmpty) Nil
    else f(list.head, that.head) :: zipMap(list.tail)(that.tail)(f)
  }
}
