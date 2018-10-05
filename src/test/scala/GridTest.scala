import org.scalatest._
import actors._
import game.CellState

class GridTest extends FlatSpec with Matchers{
    val grid = Grid(List.fill(10,10)(CellState.EMPTY))

    // getCellState
    // -----------------------------------------------------------------------


    it should "get the state of a specific cell" in {
        grid.getCellState(1,1) shouldEqual Some(CellState.EMPTY)
    }

    it should "return None because the cell doesn't belong to the grid" in {
        grid.getCellState(11,1) shouldEqual None
    }

    // isCellBelongsTo
    // -----------------------------------------------------------------------


    it should "return true because the cell belongs to the grid" in {
        grid.isCellBelongsTo(1,1) shouldEqual true
    }

    it should "return false because the cell doesn't belong to the grid" in {
        grid.isCellBelongsTo(11,1) shouldEqual false
    }

    // updateCellState
    // -----------------------------------------------------------------------

    val gridUpCell = GridUtil.updateCellState(grid, (1,1), CellState.SHIP)

    it should "update the state of a specific cell" in {
        gridUpCell.getCellState((1,1)) shouldEqual Some(CellState.SHIP)
    }

    // updateMultipleCellsState
    // -----------------------------------------------------------------------

    val gridUpMultipleCells =  GridUtil.updateMultipleCellsState(grid, Set((1,1), (1,3), (1,5)), CellState.SHIP)

    it should "update the state of multiple cells" in {
        (
            gridUpMultipleCells.getCellState((1,1)) shouldEqual Some(CellState.SHIP),
            gridUpMultipleCells.getCellState((1,3)) shouldEqual Some(CellState.SHIP),
            gridUpMultipleCells.getCellState((1,5)) shouldEqual Some(CellState.SHIP)
        )
    }

    it should "return the initial grid because one of the cell doesn't belong to the grid" in {
        GridUtil.updateMultipleCellsState(grid, Set((1,1), (1,12), (1,5)), CellState.SHIP) shouldEqual grid
    }

    it should "return the initial grid because the set of cell is empty" in {
        GridUtil.updateMultipleCellsState(grid, Set(), CellState.SHIP) shouldEqual grid
    }
}
