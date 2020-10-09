class TicTacToe(_n: Int) {
  private val i1 = -(_n * 2 + 1)
  val tictactoe: Seq[Array[Int]] = Array.fill(_n, _n)(i1).toSeq

  def move(row: Int, col: Int, player: Int): Int = {
    if (tictactoe(row)(col) == i1) tictactoe(row)(col) = player

    if (chk(row, col, Set(player))) player
    else 0
  }

  def chk(row: Int, col: Int, cmp: Set[Int]): Boolean = {
    val range = 0 until _n

    if (tictactoe(row).toSet == cmp
      || range.map(i => tictactoe(i)(col)).toSet == cmp
      || range.map(i => tictactoe(i)(i)).toSet == cmp
      || range.map(i => tictactoe(_n - i - 1)(i)).toSet == cmp) {
      return true
    }
    false
  }
}

//var obj = new TicTacToe(2)
//var param_1 = obj.move(0, 1, 1)
//var param_2 = obj.move(1, 1, 2)
//var param_3 = obj.move(1, 0, 1)
//List(null, param_1, param_2, param_3)
