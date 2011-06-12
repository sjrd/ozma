object MergeSort {
  def main(args: Array[String]) {
    val list = args.toList map (_.toInt)
    val sorted = mergeSort(list)
    sorted foreach println
  }

  def mergeSort[A](list: List[A])(
      implicit cmp: Ordering[_ >: A]): List[A] = {
    list match {
      case Nil => Nil
      case head :: Nil => list
      case _ =>
        val (left, right) = split(list)
        merge(mergeSort(left), mergeSort(right))
    }
  }

  def split[A](list: List[A], leftAcc: List[A] = Nil,
      rightAcc: List[A] = Nil): (List[A], List[A]) = {
    if (list.isEmpty) (leftAcc, rightAcc)
    else split(list.tail, list.head :: rightAcc, leftAcc)
  }

  def merge[A](left: List[A], right: List[A])(
      implicit cmp: Ordering[_ >: A]): List[A] = {
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else {
      if (cmp.lteq(left.head, right.head))
        left.head :: merge(left.tail, right)
      else
        right.head :: merge(left, right.tail)
    }
  }
}
