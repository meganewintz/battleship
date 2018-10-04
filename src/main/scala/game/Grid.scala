package game

import actors.Ship

class Grid(size: Int) {

  /*
  Norm :
  - cells available = "-"
  - cells occupied = ship number
  - cells touched = "x"
  - cells touched and occupied by a ship = "o"
   */

    // The grid is an array of CellState
//    var grid = Array.fill(size,size)(CellState.EMPTY)
//
//  override def toString: String = {
//    val letters = List("A")
//    "   1   2   3   4   5   6   7   8   9   10 \n" +
//        //grid foreach { row => "A" + row foreach { col => " [ " + col + " ] "}; "\n" } +
//        //grid.map(_.mkString("A ", "   ", "")).mkString("[  ", "\n", "  ]")
//    grid.map( row => row.mkString(grid.indexOf(row).toChar.toString + "   ", "   ", "")).mkString("\n")
//
//  }
//
//    /**
//      * Update the state of one cell
//      * @param cell
//      * @return the grid updated
//      */
//    def updateCellState(cell: Array[Int]): Grid = ???

//  override def toString: String = {
//    grid foreach { row => row foreach print; print("  x"); println }
//  }

}


