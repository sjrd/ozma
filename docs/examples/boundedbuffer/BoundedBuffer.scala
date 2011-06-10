import scala.ozma._

object BoundedBuffer {
  val TimeUnit = 500

  def main(args: Array[String]) {
    val produced = generate(1)
    val buffered = threadGC(produced)(boundedBuffer(_, 5))

    println(buffered(0))

    sleep(8*TimeUnit)
    for (i <- 1 to 10)
      println(buffered(i))
  }

  def generate(from: Int): List[Int] = byNeedFuture {
    sleep(TimeUnit)
    from :: generate(from+1)
  }

  def boundedBuffer[A](input: List[A], capacity: Int): List[A] = {
    def loop(input: List[A], inputAhead: List[A]): List[A] = byNeedFuture {
      input.head :: loop(input.tail, thread(inputAhead.tail))
    }

    val inputAhead = thread(input drop capacity)
    loop(input, inputAhead)
  }
}
