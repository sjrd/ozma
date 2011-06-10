package ozma

object Random {
  @native def rand(): Int

  def rand(upper: Int): Int = rand() % upper

  def rand[A](choices: Seq[A]): A = choices(rand(choices.length))
}
