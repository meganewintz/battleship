package actors
import game.Utility._
import game._

//// position must belongs to the value of the objet Direction
//// coord of startPoint must be included in [0, GridSize]
//class Ship(direction: String, startPoint: Cell, size: Int) {
//
//    val coord: List[Cell] = {
//    var c = new Array[Cell](size)
//    direction match {
//      case Direction.HORIZONTAL =>  List.iterate(startPoint, size)(p => new Cell(p.x, p.y+1))
//      case Direction.VERTICAL =>  List.iterate(startPoint, size)(p => new Cell(p.x+1, p.y))
//      case _ => throw new ExceptionInInitializerError("The direction is incorrect.")
//      }
//    }
//
//    /**
//      *
//      * @param coord: the coord that we want to check
//      * @return true if the coord correspond to one of the ship coor
//      */
//    def isTouched(coord: Array[Int]): Boolean = ???
//
//    /**
//      * Test if the ship is sunk
//      * @return true if the the sunk, else false
//      */
//    def isSunk: Boolean = coord.isEmpty
//
//    /**
//      * Delete the cell specified in param
//      * @param cell: the cell that we want to delete
//      * @return the ship updated
//      */
//    def deleteCell(cell: Array[Int]): Ship = ???
//
//}


case class Ship(name: String, position: Set[Tuple2[Int,Int]] = Set()) {

    /**
      * Check if the square belongs to the ship.
      *
      * @param square. The square to test.
      * @return Boolean. True if the square belongs to the ship.
      */
    def isTouched(square: Tuple2[Int,Int]): Boolean = {
        position.contains(square)
    }

    /**
      * Check if the ship is sunk.
      *
      * @return Boolean. True if the ships is sunk.
      */
    def isSunk: Boolean = position.isEmpty

    /**
      * The actual size of the ship
      *
      * @return the actual size of the ship.
      */
    def size: Int = position.size

}

// Tout ce qui renvoie un new ship
object ShipUtil {

    /**
      * Create a new ship without the square given.
      * If the square doesn't belong to the ship, simply return the ship.
      *
      * @param ship
      * @param square
      * @return a new ship that contains all the elements of this ship but that not contains square.
      */
    def removeSquareShip(ship: Ship, square: Tuple2[Int,Int]): Ship = {

        if (ship.isTouched(square)) {
            val newPosition = ship.position - square
            ship.copy(position = newPosition)
        }
        else ship
    }

}

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
        "\n\n"
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

        if (cells.isEmpty || cells.forall(c => !grid.isCellBelongsTo(c))) grid
        else {
            val newGrid = updateCellState(grid, cells.head, newState)
            updateMultipleCellsState(newGrid, cells.tail, newState)
        }
    }
}

case class Player(
                 name: String,
                 score: Int = 0,
                 fistPlayer: Boolean = false,
                 shipsGrid: Grid = Grid(List.fill(10,10)(CellState.EMPTY)),
                 shootsGrid: Grid = Grid(List.fill(10,10)(CellState.EMPTY)),
                 fleet: List[Ship] = List()
                 ) {

    /**
      * get the state of the cell, of the shipsGrid.
      *
      * @param cell
      * @return an option value containing the state of the cell. None if cell doesn't belong to the shipsGrid.
      */
    def getCellStateShipsGrid(cell: Tuple2[Int, Int]): Option[CellState.Value] = {
        shipsGrid.getCellState(cell)
    }

    /**
      * Check if all the squares of the ship are correct: the coords belong to the grid and not already occupied by a ship.
      *
      * @param ship
      * @return true if the ship is placeable, else false.
      */
    def isShipPlaceable(ship: Ship): Boolean = {
        ship.position.forall(p => shipsGrid.isCellBelongsTo(p) && this.shipsGrid.cells(p._1)(p._2).equals(CellState.EMPTY))
    }

    /**
      * Find the ship that contains the cell.
      *
      * @param cell
      * @return an option value containing the ship touched. None if no ship was touched.
      */
    def shipTouched(cell: Tuple2[Int,Int]): Option[Ship] = {
        fleet.find( x => x.isTouched(cell))
    }
    /**
      * Check if the ship is sunk.
      *
      * @param ship
      * @return true if the ship is sunk. Else false
      */
    def isShipSunk(ship: Ship): Boolean = {
        if (fleet.contains(ship)) { ship.isSunk}
        else false
    }

    /**
      * Check if the fleet is sunk.
      *
      * @return true if the fleet is sunk.
      */
    def isFleetSunk(): Boolean = fleet.isEmpty

}


// Tout ce qui renvoie un new ship
object PlayerUtil {

    /**
      * Create a new player by incrementing the score of the player + 1.
      * @param player
      * @return a new player with a score +1.
      */
    def incrementScore(player: Player): Player = {
        val oldScore = player.score
        player.copy(score = oldScore + 1)
    }

    /**
      * Create a new player by resseting player. Keep: name and score. If the user was not firstPlayer, he becomes it.
      * @param player
      * @return a new player ressenting.
      */
    def ressetPlayer(player: Player): Player = {

        if (player.fistPlayer) {
            player.copy(fleet = List(), shipsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), shootsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), fistPlayer = false)
        }
        else {
            player.copy(fleet = List(), shipsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), shootsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), fistPlayer = true)
        }
    }

    /**
      * Create a new Player with a new ship added.
      * The ship is added only if it is placeable.
      * If it is not placeable, simply return the player.
      *
      * @param player
      * @param ship
      * @return a new player with a ship added, if it is placeable.
      */
    def addShipToPlayer(player: Player, ship: Ship): Player = {
        if (player.isShipPlaceable(ship)) {

            // Add the ship position on the shipsGrid.
            val shipsGrid = player.shipsGrid
            val newShipsGrid = GridUtil.updateMultipleCellsState(player.shipsGrid, ship.position, CellState.SHIP)

            // Add it on the fleet.
            val newFleet = ship :: player.fleet

            // Create a new player.
            player.copy(fleet = newFleet, shipsGrid = newShipsGrid)
        }
        else player
    }

    /**
      * Create a new Player without a specific ship in its fleet.
      *
      * @param player
      * @param ship
      * @return a new player without the ship specify in its fleet.
      */
    def removeShipToPlayer(player: Player, ship: Ship): Player = {
        val newFleet = player.fleet.filterNot(x => x == ship)
        player.copy(fleet = newFleet)
    }

    /**
      * create a new Player with one of the cell of its shipsGrid updated.
      * The cell must belongs to the shipsGrid.
      * If not, the initial player is return.
      *
      * @param player
      * @param cell
      * @param newState
      * @return a new Player with one of the cell of its shipsGrid updated.
      */
    def updateShipsGrid(player: Player, cell: Tuple2[Int, Int], newState: CellState.Value): Player = {
        val newGrid = GridUtil.updateCellState(player.shipsGrid, cell, newState)
        player.copy(shipsGrid = newGrid)

    }

    /**
      * create a new Player with one of the cell of its shootsGrid updated.
      * The cell must belongs to the shootsGrid.
      * If not, the initial player is return.
      *
      * @param player
      * @param cell
      * @param newState
      * @return a new Player with one of the cell of its shootsGrid updated.
      */
    def updateShootsGrid(player: Player, cell: Tuple2[Int, Int], newState: CellState.Value): Player = {
        val newGrid = GridUtil.updateCellState(player.shootsGrid, cell, newState)
        player.copy(shootsGrid = newGrid)

    }

    /**
      * Create a new player by removing the square of the ship touched.
      * If the ship is sunk, removed it from the fleet.
      * If no ship was touched, simply return the player.
      *
      * @param player
      * @param cell
      * @return a new player with the ship touched updated.
      */
    def fleetTouched(player: Player, cell: Tuple2[Int, Int]): Player = {

        val shipTouched = player.shipTouched(cell)

        // If there is not ship touched.
        if (shipTouched.isEmpty) player
        else {
            val newShip = ShipUtil.removeSquareShip(shipTouched.get, cell)

            // If the ship is sunk, we remove it.
            if (newShip.isSunk) {
                val newFleet = player.fleet.filterNot( s => s == shipTouched.get)
                player.copy(fleet = newFleet)
            }
            // Else we update it.
            else {
                val newFleet = player.fleet.updated(player.fleet.indexOf(shipTouched.get), newShip)
                player.copy(fleet = newFleet)
            }
        }
    }

    // A voir si je ne regroupe pas les deux
    // Agit sur notre grille de ships. On note la position du tire adverse
    /**
      * Create a new player
      *
      * @param player
      * @param cell
      * @return a new player
      */
    def addAdverseShoot(player: Player, cell: Tuple2[Int, Int]): Player = {

        val cellState = player.getCellStateShipsGrid(cell)

        if (cellState.isEmpty) player
        else {
            cellState.get match {
                // If there is no ship, we mark miss on our shipsGrid
                case CellState.EMPTY => PlayerUtil.updateShipsGrid(player, cell, CellState.MISS)
                // If there is a ship, we update the fleet and the shipsGrid
                case CellState.SHIP => {
                    fleetTouched(player, cell)
                    PlayerUtil.updateShipsGrid(player, cell, CellState.TOUCH)
                }
                // Else, we do nothing
                case _ => player
            }
        }
    }
}
//object Ship {
//
//}
/*

Ship
- isTouched(coord)          OK
- isSunk                    OK
- removeCoord(coord)        OK

Grid
- getCellState(coord)       OK // gérer le cas ou les coord sont ext à la grille
- updateCellState(coord)    OK // gérer le cas ou les coord sont ext à la grille

Player
- addShip(ship)             OK
- removeShip(ship)          OK
- updateShip(ship)          OK
- updateGrid                OK






In a functional language, a functions are first-class citizens.

This means: a function has a value even if it is not applied.

- Faire le lien coord A1 vers Point(0,0)
- isTouched
- isSunk
- update shipGrid j2
- update shootGrid j1

GRID:
- display grid
- updateCell

1.  add 5 ships j1
    add 5 ships j2
2.  j1 play:
        display shipsGrod
        display shootGrid
        enter coord
        check coord on the shipsGrid j2
        - coord = TOUCH or MISS --> already touched, try again
        - coord = EMPTY --> update StateCell(MISS)
        - coord = SHIP
                    - updateStateCell(TOUCH)
                    - delete cell ship touched
                    - check if the ship is sunk
                        - check if the party is finished
    j2 play
3. display j winner


Idea cell:
A cell has :
    - x
    - y
    - value
    - state

def a state and a value on each cell.
- state = EMPTY -> transform it to MISS
- state = MISS or HIT -> enter coord again
- state = SHIP -> check in the fleet which ship was touched

state = EMPTY -> value = "-"
    state = MISS -> value = "x"
    state = HIT -> value = "o"
    state = EMPTY -> value = "-"
    state = SHIP -> value = ??

If there is a ship, check in the fleet which ship was touched
 */

