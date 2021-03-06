import org.scalatest._
import basis._
import game.CellState

class GridTest extends FlatSpec with Matchers{
    val grid = Grid(List.fill(10,10)(CellState.EMPTY))

    // getCellState
    // -----------------------------------------------------------------------

    it should "get the state of a specific cell" in {
        grid.getCellState(Cell(1,1)) shouldEqual Some(CellState.EMPTY)
    }

    it should "return None because the cell doesn't belong to the grid" in {
        grid.getCellState(Cell(11,1)) shouldEqual None
    }

    // isCellBelongsTo
    // -----------------------------------------------------------------------


    it should "return true because the cell belongs to the grid" in {
        grid.isCellBelongsTo(Cell(1,1)) shouldEqual true
    }

    it should "return false because the cell doesn't belong to the grid" in {
        grid.isCellBelongsTo(Cell(11,1)) shouldEqual false
    }

    // updateCellState
    // -----------------------------------------------------------------------

    val gridUpCell = grid.updateCellState(Cell(1,1), CellState.SHIP)

    it should "update the state of a specific cell" in {
        gridUpCell.getCellState(Cell(1,1)) shouldEqual Some(CellState.SHIP)
    }

    // updateMultipleCellsState
    // -----------------------------------------------------------------------

    val gridUpMultipleCells =  grid.updateMultipleCellsState(Set(Cell(1,1), Cell(1,3), Cell(1,5)), CellState.SHIP)

    it should "update the state of multiple cells" in {
        (
            gridUpMultipleCells.getCellState(Cell(1,1)) shouldEqual Some(CellState.SHIP),
            gridUpMultipleCells.getCellState(Cell(1,3)) shouldEqual Some(CellState.SHIP),
            gridUpMultipleCells.getCellState(Cell(1,5)) shouldEqual Some(CellState.SHIP)
        )
    }

    it should "return the initial grid because one of the cell doesn't belong to the grid" in {
        grid.updateMultipleCellsState(Set(Cell(1,1), Cell(1,12), Cell(1,5)), CellState.SHIP) shouldEqual grid
    }

    it should "return the initial grid because the set of cell is empty" in {
        grid.updateMultipleCellsState(Set(), CellState.SHIP) shouldEqual grid
    }
}
