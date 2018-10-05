package actors

import game.CellState

case class Grid(cells: List[List[CellState.Value]]) {

    /**
      * Get the actual state of the cell if the cell belongs to the grid.
      *
      * @param cell
      * @return an option value containing the cell state. None if the cell doesn't belong to the grid.
      */
    def getCellState(cell: Tuple2[Int,Int]): Option[CellState.Value] = {
        if (isCellBelongsTo(cell)) {
            Some(cells.apply(cell._1).apply(cell._2))
        }
        else None
    }

    /**
      * Check if the cell eblongs to the grid.
      *
      * @param cell
      * @return true if the cell belongs to the grid.
      */
    def isCellBelongsTo(cell: Tuple2[Int,Int]): Boolean = {
        val size = cells.length
        cell._1 >= 0 && cell._1 < size && cell._2 >= 0 && cell._2 < size
    }

    override def toString: String = {
        val letters = List("A")
        "\n"
        "    A   B   C   D   E   F   G   H   I   J \n" +
            //grid foreach { row => "A" + row foreach { col => " [ " + col + " ] "}; "\n" } +
            //grid.map(_.mkString("A ", "   ", "")).mkString("[  ", "\n", "  ]")
            cells.map( row => row.mkString(cells.indexOf(row).toString + "   ", "   ", "")).mkString("\n") +
            "\n"
    }
}

// Tout ce qui renvoie un new ship
object GridUtil {

    /**
      * Create a new grid with the cell state enter updated.
      * If the cell doesn't belong to the grid, simply return the grid.
      *
      * @param grid
      * @param cell
      * @param newState
      * @return a new grid with the cell state enter updated.
      */
    def updateCellState(grid: Grid, cell: Tuple2[Int, Int], newState: CellState.Value): Grid = {
        if (grid.isCellBelongsTo(cell)) {
            val rowCellConcerned = grid.cells.apply(cell._1)
            val newRowCellConcerned = rowCellConcerned.updated(cell._2, newState)
            val newCells = grid.cells.updated(cell._1, newRowCellConcerned)
            grid.copy(cells = newCells)
        }
        else grid

    }

    /**
      * Create a new grid with all the cells enter updated by the newState.
      * All of the cells must belongs to the grid.
      * If one of the cells doesn't belong to the grid, simply return the grid.
      *
      * @param grid
      * @param cells
      * @param newState
      * @return a new grid with all the cells enter updated by the newState.
      */
    def updateMultipleCellsState(grid: Grid, cells: Set[Tuple2[Int, Int]], newState: CellState.Value): Grid = {

        if (cells.isEmpty || cells.exists(c => !grid.isCellBelongsTo(c))) grid
        else {
            val newGrid = updateCellState(grid, cells.head, newState)
            updateMultipleCellsState(newGrid, cells.tail, newState)
        }
    }
}


