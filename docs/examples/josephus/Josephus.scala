import scala.ozma._
import ozma._

object Josephus {
  def main(args: Array[String]) {
    val count = args(0).toInt
    val step = args(1).toInt
    println(josephus(count, step))
    println(declarativeJosephus(count, step))
  }

  def josephus(count: Int, step: Int) = {
    class Victim(val id: Int) {
      private var alive: Boolean = true
      private var succ: Victim = _
      private var _pred: Victim = _

      def pred = _pred
      def pred_=(value: Victim) {
        _pred = value
        _pred.succ = this
      }

      def kill(i: Int, survivors: Int): Int = {
        if (alive) {
          if (survivors == 1) {
            id
          } else if (i % step == 0) {
            alive = false
            succ.pred = pred
            succ.kill(1, survivors-1)
          } else {
            succ.kill(i+1, survivors)
          }
        } else
          succ.kill(i, survivors)
      }
    }

    val victims = for (id <- (1 to count).toList)
      yield ResultPort.newActiveObject(new Victim(id))

    val first = victims.head
    val last = victims.tail.foldLeft(first) { (pred, victim) =>
      victim.pred = pred
      victim
    }
    first.pred = last

    victims.head.kill(1, count)
  }

  def declarativeJosephus(count: Int, step: Int) = {
    type KillMsg = (Int, Int)

    val last: Int

    def victim(id: Int, stream: List[KillMsg]): List[KillMsg] = {
      if (stream.isEmpty) Nil
      else {
        val (i, survivors) = stream.head

        if (survivors == 1) {
          last = id
          Nil
        } else if (i % step == 0)
          (1, survivors-1) :: stream.tail
        else
          (i+1, survivors) :: victim(id, stream.tail)
      }
    }

    val initKillMsg = (1, count)

    val lastStream: List[KillMsg]
    lastStream = pipe(initKillMsg :: lastStream, 1, count)(victim)

    last
  }

  private def pipe[A](stream: List[A], id: Int, count: Int)(
      handler: (Int, List[A]) => List[A]): List[A] = {
    if (id > count) stream
    else pipe(thread(handler(id, stream)), id+1, count)(handler)
  }
}
