package actors
import game._

class Cell(val x: Int, val y: Int, var state: CellState.Value = CellState.EMPTY ) {

    /**
      * Update the state of the Cell
      * @param newState
      * @return
      */
    def updateState(newState: CellState.Value): CellState.Value = ???
        // can update the cell or create a new one?
}

