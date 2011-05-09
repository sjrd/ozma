object Echo {
  def main(args: Array[String]) {
    def loop(i: Int) {
      if (i < args.length) {
        Console.println(args(i))
        loop(i+1)
      }
    }

    loop(0)
  }
}
