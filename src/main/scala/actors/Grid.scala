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
        "\n" +
        "   A   B   C   D   E   F   G   H   I   J \n" +
        cells.zipWithIndex.map{ case (e, i) => e.mkString("   ", "   ", "   ") + (i+1)}.mkString("\n") +
        "\n"
    }

    /**
      * Create a new grid with the cell state enter updated.
      * If the cell doesn't belong to the grid, simply return the grid.
      *
      * @param cell
      * @param newState
      * @return a new grid with the cell state enter updated.
      */
    def updateCellState(cell: Tuple2[Int, Int], newState: CellState.Value): Grid = {
        if (isCellBelongsTo(cell)) {
            val rowCellConcerned = cells.apply(cell._1)
            val newRowCellConcerned = rowCellConcerned.updated(cell._2, newState)
            val newCells = cells.updated(cell._1, newRowCellConcerned)
            copy(cells = newCells)
        }
        else this

    }

    /**
      * Create a new grid with all the cells enter updated by the newState.
      * All of the cells must belongs to the grid.
      * If one of the cells doesn't belong to the grid, simply return the grid.
      *
      * @param cells
      * @param newState
      * @return a new grid with all the cells enter updated by the newState.
      */
    def updateMultipleCellsState(cells: Set[Tuple2[Int, Int]], newState: CellState.Value): Grid = {

        if (cells.isEmpty || cells.exists(c => !isCellBelongsTo(c))) this
        else {
            val newGrid = updateCellState(cells.head, newState)
            newGrid.updateMultipleCellsState(cells.tail, newState)
        }
    }
}



