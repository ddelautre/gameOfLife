import GameOfLife.nextGen
import Grid._
import org.scalatest.FunSuite

class GameOfLifeTest extends FunSuite {

  test("A cell with 3 neighbours is born") {
    val grid = parse(
      """
        |...
        |..*
        |.**
        |...
        |.**
        |..*
        |""")
    val result =
      """
        |...
        |.**
        |.**
        |...
        |.**
        |.**
        |""".stripMargin

    assert(prettyPrint(nextGen(grid)) === result)
  }

  test("A cell with 3 neighbours survives") {
    val grid = parse(
      """
        |**
        |**
        |""")
    val result =
      """
        |**
        |**
        |""".stripMargin

    assert(prettyPrint(nextGen(grid)) === result)
  }

  test("A cell with 2 neighbours survives") {
    val grid = parse(
      """
        |***
        |""")
    val result =
      """
        |.*.
        |""".stripMargin

    assert(prettyPrint(nextGen(grid)) === result)
  }

  test("A cell with 4 neighbours dies") {
    val grid = parse(
      """
        |..*..
        |.*.*.
        |*.*.*
        |.*.*.
        |..*..
        |""")
    val result =
      """
        |..*..
        |.*.*.
        |*...*
        |.*.*.
        |..*..
        |""".stripMargin


    assert(prettyPrint(nextGen(grid)) === result)
  }

  test("A cell with 0 neighbour dies") {
    val grid = parse(
      """
        |..
        |.*
        |""")
    val result =
      """
        |..
        |..
        |""".stripMargin


    assert(prettyPrint(nextGen(grid)) === result)
  }

  test("A cell with 1 neighbour dies") {
    val grid = parse(
      """
        |..
        |**
        |""")
    val result =
      """
        |..
        |..
        |""".stripMargin


    assert(prettyPrint(nextGen(grid)) === result)
  }

  test("A cell with 5 neighbours dies") {
    val grid = parse(
      """
        |***
        |***
        |""")
    val result =
      """
        |*.*
        |*.*
        |""".stripMargin


    assert(prettyPrint(nextGen(grid)) === result)
  }
}
