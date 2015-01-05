import Grid._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GridTest extends FunSuite with ShouldMatchers {

  test("it should parse an empty grid") {
    assert(parse("") === Grid(List()))
  }

  test("it should parse a dead cell") {
    assert(parse(".") === Grid(List(List(false))))
  }

  test("it should parse a living cell'") {
    assert(parse("*") === Grid(List(List(true))))
  }

  test("it should parse a single row with living and dead cells") {
    assert(parse("*.*") === Grid(List(List(true, false, true))))
  }

  test("it should parse multiple rows") {
    val grid = parse(
      """
        |.
        |.
        |""")
    grid.shouldEqual(Grid(List(List(false), List(false))))
  }

  test("it should parse any grid") {
    val grid = parse(
      """
        |...
        |..*
        |.**
        |""")
    grid.shouldEqual(Grid(List(List(false, false, false), List(false, false, true), List(false, true, true))))
  }

  test("it should print an empty grid") {
    assert(print(Grid(List(List()))) === "")
  }

  test("it should print a dead cell") {
    assert(print(Grid(List(List(false)))) === ".")
  }

  test("it should print a living cell") {
    assert(print(Grid(List(List(true)))) === "*")
  }

  test("it should print a row with living and dead cells") {
    assert(print(Grid(List(List(true, false, true)))) === "*.*")
  }

  test("it should print any grid") {
    val grid =
      """...
        |..*
        |.**""".stripMargin
    assert(print(Grid(List(List(false, false, false), List(false, false, true), List(false, true, true)))) === grid)
  }

  test("it should bring cells alive") {
    var grid = parse(
      """
        |...
        |...
        |...
        |""")

    val result =
      """
        |***
        |***
        |***
        |""".stripMargin

    for (row <- 0 to 2; col <- 0 to 2) {
      grid = grid.mutate(Cell(row, col))
    }

    assert(prettyPrint(grid) === result)
  }

  test("it should kill cells") {
    var grid = parse(
      """
        |***
        |***
        |***
        |""")

    val result =
      """
        |...
        |...
        |...
        |""".stripMargin


    for (row <- 0 to 2; col <- 0 to 2) {
      grid = grid.mutate(Cell(row, col))
    }

    assert(prettyPrint(grid) === result)
  }

  test("knows a cell living neighbors") {
    val grid = parse(
      """
        |...
        |*..
        |**.
        |""")
    assert(grid.countNeighbors(Cell(1, 0)) === 2)
  }

  test("calculates positions around a cell") {
    val grid = parse( """
                        |...
                        |...
                        |...
                        |""")

    assert(grid.around(Cell(1, 1)) === List(
      Cell(0, 0), Cell(0, 1), Cell(0, 2),
      Cell(1, 0), Cell(1, 2),
      Cell(2, 0), Cell(2, 1), Cell(2, 2)
    ))
  }

  test("retains valid positions in the grid") {
    val grid = parse(
      """
        |...
        |...
        |...
        |""")

    assert(grid.neighbors(Cell(0, 0)) === List(Cell(0, 1), Cell(1, 0), Cell(1, 1)))
    assert(grid.neighbors(Cell(2, 2)) === List(Cell(1, 1), Cell(1, 2), Cell(2, 1)))
  }

}
