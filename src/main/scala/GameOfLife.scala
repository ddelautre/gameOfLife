import Grid._

object GameOfLife {

  def nextGen(grid: String): String = print(nextGen(parse(grid)))

  def nextGen(grid: Grid): Grid = {
    def willBeBorn(cell: Cell): Boolean = grid.countNeighbors(cell) == 3 && !grid.alive(cell)

    def willDie(cell: Cell): Boolean = {
        val neighbors: Int = grid.countNeighbors(cell)
        (neighbors > 3 ||  neighbors < 2) && grid.alive(cell)
    }

    grid.mutate(cell => willBeBorn(cell) || willDie(cell))
  }

}
