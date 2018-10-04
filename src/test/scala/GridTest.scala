import org.scalatest._
import actors._
import game.CellState

class GridTest extends FlatSpec with Matchers{
    val grid = Grid(List.fill(10,10)(CellState.EMPTY))

    it should "get the state of a specific cell" in {
        grid.getCellState(1,1) shouldEqual CellState.EMPTY
    }

    val gridUpCell = GridUtil.updateCellState(grid, (1,1), CellState.SHIP)

// A verif car on ne check pas vraiment l'équalité total de l'objet grid mais seulement de la case supposée changer
    it should "update the state of a specific cell" in {
        gridUpCell.getCellState((1,1)) shouldEqual CellState.SHIP
    }

    val gridUpMultipleCells =  GridUtil.updateMultipleCellsState(grid, Set((1,1), (1,3), (1,5)), CellState.SHIP)

// A verif car on ne check pas vraiment l'équalité total de l'objet grid mais seulement des cases supposées changer
    it should "update the state of multiple cells" in {
        (
            gridUpMultipleCells.getCellState((1,1)) shouldEqual CellState.SHIP,
            gridUpMultipleCells.getCellState((1,3)) shouldEqual CellState.SHIP,
            gridUpMultipleCells.getCellState((1,5)) shouldEqual CellState.SHIP
        )

    }
}
