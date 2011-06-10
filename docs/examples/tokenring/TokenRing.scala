import scala.ozma._

object TokenRing {
  def main(args: Array[String]) {
    val count = args(0).toInt
    createTokenAgents(count)

    while (true) sleep(1000)
  }

  def createTokenAgents(count: Int) {
    def loop(id: Int, input: List[Unit]): List[Unit] = {
      if (id > count) input
      else loop(id+1, thread(tokenAgent(id, input)))
    }

    val bootstrap: List[Unit]
    val output = loop(1, bootstrap)
    bootstrap = () :: output
  }

  def tokenAgent(id: Any, inTokens: List[Unit]): List[Unit] = {
    if (inTokens.isEmpty) Nil
    else {
      println(id + " has the token")
      sleep(1000)
      inTokens.head :: tokenAgent(id, inTokens.tail)
    }
  }
}
