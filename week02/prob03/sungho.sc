class TicTacToe(val _n: Int) {
  val cntRows = Array.fill(2, _n)(0)
  val cntCols = Array.fill(2, _n)(0)
  val cntDiag = Array.fill(2)(0)
  val cntDiagRev = Array.fill(2)(0)

  def move(row: Int, col: Int, player: Int): Int = {
    val playerIdx = player - 1

    cntCols(playerIdx)(col) += 1
    cntRows(playerIdx)(row) += 1
    if (row == col) cntDiag(playerIdx) += 1
    if (row + col == _n - 1) cntDiagRev(playerIdx) += 1
    val candidates = List(
      cntCols(playerIdx)(col),
      cntRows(playerIdx)(row),
      cntDiag(playerIdx),
      cntDiagRev(playerIdx)
    )
    if(candidates.contains(_n))
      player
    else
      0
  }
}
