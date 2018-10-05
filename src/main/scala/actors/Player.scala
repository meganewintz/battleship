package actors
import game.CellState

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
      * get the state of the cell, of the shootsGrid.
      *
      * @param cell
      * @return an option value containing the state of the cell. None if cell doesn't belong to the shootsGrid.
      */
    def getCellStateShootsGrid(cell: Tuple2[Int, Int]): Option[CellState.Value] = {
        shootsGrid.getCellState(cell)
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
      * NOT change its grids, only fleet.
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
      * NOT change its grids, only affect its fleet.
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
            // Else we update the fleet with the new ship.
            else {
                val newFleet = player.fleet.updated(player.fleet.indexOf(shipTouched.get), newShip)
                player.copy(fleet = newFleet)
            }
        }
    }

    /**
      * Create a new player added the shoot of the opponent.
      * Update shipsGrid.
      * Update fleet if a ship is touched.
      * Simply return the player if the cell was already touched or doesn't not belong to shipsGrid.
      *
      * @param player
      * @param cell
      * @return a new player
      */
    def addOpponentShoot(player: Player, cell: Tuple2[Int, Int]): Player = {

        val cellState = player.getCellStateShipsGrid(cell)

        if (cellState.isEmpty) player
        else {
            cellState.get match {
                // If there is no ship, we mark miss on our shipsGrid
                case CellState.EMPTY => PlayerUtil.updateShipsGrid(player, cell, CellState.MISS)
                // If there is a ship, we update the fleet and the shipsGrid
                case CellState.SHIP => {
                    val newPlayer = fleetTouched(player, cell)
                    PlayerUtil.updateShipsGrid(newPlayer, cell, CellState.TOUCH)
                }
                // Else, we do nothing
                case _ => player
            }
        }
    }
}