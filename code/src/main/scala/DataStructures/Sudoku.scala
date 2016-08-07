package DataStructures

/**
  * Created by greddy on 8/6/16.
  */

import scala.language.postfixOps

object Sudoku {


  type Element = (Int, Int)

  def row(element: Element): Int = element._1 / 9
  def column(element: (Int, Int)): Int = element._1 % 9

  def tile(element: (Int, Int)): Int = {
    val j = column(element)/3
    val i = row(element) /3
    j+3*i
  }


 /*
 Check for whether an element contains
  */
  def checkX(board: List[Element], next: Element): Boolean = {

    def check(b: Boolean, tuple: Element): Boolean = {
      b && ((row(tuple) == row(next) && tuple._2 != next._2) || row(tuple) != row(next))
    }

    board.foldLeft(true)(check)
  }

  /*
   TASK
   check columns for uniqueness

   In a valid Sudoku board,
   each column contains only one copy of each digit 1-9.
   This is the same rule as in `checkX`, but for columns.

   Check that each column satisfies this requirement.
   */
  def checkY(board: List[Element], next: Element): Boolean = {

    // Fold Left syntax f(B,A) => B ( A is of the type of List)
    def check(b:Boolean, elem:Element):Boolean = {
      b && ( (column(elem)== column(next) && elem._2 != next._2) || column(elem) != column(next) )
    }

    board.foldLeft(true)(check)
  }

  /*
   TASK 1b
   check tiles for uniqueness

   A Sudoku board contains 9 3x3 tiles.

   As with each row and each column of the board,
   each tile must contain only one copy of each digit 1-9.

   */
  def checkT(board: List[Element], next: Element): Boolean = {

    def check(b:Boolean, elem:Element):Boolean = {
      b && (next._2 != elem._2) && (next._1 != elem._1)
    }

    board.foldLeft(true)(check)
  }

  /*
   TASK 1c
   check that a given position has not been filled
   */
  def notPlayed(board: List[Element], index: Int): Boolean = {

    def check(b:Boolean, elem:Element): Boolean = b && (elem._1 != index)

    board.foldLeft(true)(check)
  }

  /*
   TASK 1d
   check that a given position is legal with respect to checkX, checkY, checkT
   */
  def isLegal(board: List[Element], next: Element): Boolean =
    checkX(board,next) && checkY(board,next) && checkT(board,next)

  def isLegalBoard(board: List[Element]):Boolean = {

    /*
    def check(b:Boolean,elem:Element): Boolean =
        b && isLegal(board,elem)

    board.foldLeft(true)(check)
    */

    if (board.isEmpty) true
    else isLegal(board.tail,board.head)
  }

  def isCompleteBoardLegal(board:List[Element]):Boolean = {
    if (board.size != 81) false
    else isLegalBoard(board)
  }


  //recursively provide all solutions to puzzle w/ given initial conds
  def sudokuSolve(initial: List[Element]): Set[List[Element]] = {

    // indices of empty board elements, given initial conditions (pre-filled board)
    val indices: List[Int] =
      (0 until 81) filter { index => notPlayed(initial, index)} toList

    /*
     TASK 1e
     */

    def sudokuIter(indices: List[Int]): Set[List[Element]] = {

      def internal(ind:List[Int], set:Set[List[Element]]):Set[List[Element]] = (ind,set) match {
        case (Nil, res) => res
        case (x::xs, res) => internal(xs,getValidSet(x,initial)).union(set)
      }

      def getValidSet2(index:Int, set: Set[List[Element]]):Set[List[Element]] ={
        val res = for (board <- set) yield getValidSet(index,board)
        res.reduce(_ union _)
      }

      def getValidSet(index:Int, board: List[Element]):Set[List[Element]] = {
        (1 until 10).map { k => (index, k) :: board }.toSet.filter(isLegalBoard)
      }

      internal(indices,Set(initial))
    }

    /*
    def sudokuIter(indices: List[Int]): Set[List[Element]] = indices match {

      case Nil => Set(initial)
      case x::xs => {
        for(
          board  <- sudokuIter(xs);
          i <- (1 to 9)
          if isLegal(board, (x,i))
        ) yield (x,i)::board
      }
    }
    */
    sudokuIter(indices)
  }

  def sudokuAll(index: Int): Set[List[(Int,Int)]] = {
    if (index == -1) Set(List())
    else
      for {
        board <- sudokuAll(index-1)
        k <- 0 until 9
        if isLegal(board, (index,k))
      } yield (index,k)::board
  }


  //plotting util
  def sudokuPlot(board: List[Element]): String = {
    val out = Array.ofDim[Int](9,9)
    for {move <- board} out(move._1 / 9)(move._1 % 9) = move._2
    out.map({_.mkString(" ")}).mkString("\n")
  }

  /*

   A sample Sudoku board as a List of (position, value) tuples. Positions are in row major format.

   Row-major order
   https://en.wikipedia.org/wiki/Row-major_order

   Each element of the list below corresponds to an element in the Sudoku board.
   This board has been "flattened" into a row-major list.  The first element of each tuple is the "flattened" coordinate in the Sudoku board (explained momentarily), and the second element of each tuple is the value itself.

   First, let's establish syntax for elements of the board

   Given element M_ij,
   i is the row and j is the column

   Our row and column indices are 0-indexed, in contrast to the 1-indexed convention of a mathematical matrix.

   A Sudoku board is 9 by 9 elements.

   (0, 5) is placed at M_00
   (1, 3) is placed at M_01
   (4, 7) is placed at M_04
   (12, 1) is placed at M_13

   Here is partial plot of these four elements, with absent elements of the board filled by 0

   5 3 0 0 7 0 0 0 0
   0 0 0 1 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0

   You can print all of `initial` by running `SudokuPreview`
   */
  val initial = List((0,5),(1,3),(4,7),(9,6),(12,1),(13,9),(14,5),
    (19,9),(20,8),(25,6),(27,8),(31,6),(35,3),(36,4),
    (39,8),(41,3),(44,1),(45,7),(49,2),(53,6),(55,6),
    (60,2),(61,8),(66,4),(67,1),(68,9),(71,5),(76,8),(79,7),(80,9))

  val initial2 = List((0,5),(1,3),(4,7),      (12,1),(13,9),(14,5),
    (19,9),(20,8),       (27,8),(31,6),(35,3),
    (39,8),       (44,1),(45,7),(49,2),       (55,6),
    (60,2),(61,8),(66,4),       (68,9),(71,5),(76,8),(79,7),(80,9))

}

object SudokuPreview extends App {
  import Sudoku._


  println("find solutions for this Sudoku board:")

  println(sudokuPlot(initial))

  val test: List[(Int, Int)] = (0 until 81).toList.map(d => (d, d))

  val rows = test.map(tuple => (tuple._1, row(tuple)))

  println("rows")
  println(sudokuPlot(rows))

  val columns = test.map(tuple => (tuple._1, column(tuple)))

  println("columns")
  println(sudokuPlot(columns))

  val tiles = test.map(tuple => (tuple._1, tile(tuple)))

  println("tiles")
  println(sudokuPlot(tiles))

}

// TASK 1f
object SudokuSolver extends App {
  import Sudoku._

  println("find solutions for this Sudoku board:")
  println(sudokuPlot(initial))

  val solutionStrings: Set[String] = sudokuSolve(initial).map(board => sudokuPlot(board))

  solutionStrings.foreach { (solution: String) => println(s"solution \n $solution") }


  println("-----------------")



  println("find solutions for this Sudoku board (initial2):")
  println(sudokuPlot(initial2))

  val solutionStrings2: Set[String] = sudokuSolve(initial2).map(board => sudokuPlot(board))

  solutionStrings2.foreach { (solution: String) => println(s"solution \n $solution") }

}


