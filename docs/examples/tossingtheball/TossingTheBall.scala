import scala.ozma._
import ozma._

object TossingTheBall {
  val TimeUnit = 1000

  type Ball = Unit
  val ball: Ball = ()

  type Player = Port[Ball]

  def main(args: Array[String]) {
    val player1: Player
    val player2: Player
    val player3: Player

    player1 = makePlayer("Player 1", Seq(player2, player3))
    player2 = makePlayer("Player 2", Seq(player3, player1))
    player3 = makePlayer("Player 3", Seq(player1, player2))

    player1.send(ball)

    while (true) sleep(TimeUnit)
  }

  def makePlayer(id: Any, others: Seq[Player]): Player = {
    Port.newStatelessPortObject { ball =>
      println(id + " received the ball")
      sleep(TimeUnit)
      Random.rand(others).send(ball)
    }
  }
}
