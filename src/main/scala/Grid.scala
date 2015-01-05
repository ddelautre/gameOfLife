import Grid.State

case class Cell(row: Int, col: Int)

case class Grid(cells: List[List[State]]) {
  def around(cell: Cell): List[Cell] = {
    val around = List[(Int, Int)](
      (-1, -1), (-1, 0), (-1, 1),
      ( 0, -1),          ( 0, 1),
      ( 1, -1), ( 1, 0), ( 1, 1))

    around.map(pos => Cell(cell.row + pos._1, cell.col + pos._2))
  }

  def neighbors(cell: Cell): List[Cell] = around(cell).filter(pos => withinGrid(pos))

  def withinGrid(cell: Cell): Boolean = {
    cell.row >= 0 && cell.row < cells.size && cell.col >= 0 && cell.col < cells(cell.row).size
  }

  def countNeighbors(cell: Cell) = neighbors(cell).count(cell => alive(cell))

  def alive(cell: Cell): State = {
    cells(cell.row)(cell.col)
  }

  def mapRows[T](mapper: (List[State]) => T): List[T] = {
    cells.map(mapper)
  }

  def mutate(cell: Cell): Grid = {
    val line: List[State] = cells(cell.row)
    Grid(cells.updated(cell.row, line.updated(cell.col, !line(cell.col))))
  }

  override def toString: String = {
    mapRows(printRow).mkString("\n")
  }

  def allCells: IndexedSeq[Cell] = {
    for {row <- 0 until cells.size
         col <- 0 until cells(row).size} yield Cell(row, col)
  }

  def cellsThat(predicate: Cell => Boolean): Seq[Cell] = allCells.filter(predicate)

  def mutate(cells: Cell => Boolean): Grid = cellsThat(cells).foldLeft(this) { (mutation, cell) =>
    mutation.mutate(cell)
  }

  private def printRow(row: List[State]): String = {
    row.map {
      case true => '*'
      case false => '.'
    }.mkString
  }

}

object Grid {
  type State = Boolean

  def parse(grid: String): Grid = Grid(split(grid).map(parseRow).toList)

  def print(grid: Grid): String = grid.toString

  private def split(grid: String): Array[String] = {
    grid.stripMargin.split("\n").filterNot(_.isEmpty)
  }

  private def parseRow(row: String): List[State] = {
    row.map {
      case '.' => false
      case '*' => true
    }.toList
  }

  def prettyPrint(grid: Grid): String = {
    "\n" + Grid.print(grid) + "\n"
  }
}

