package basis
import actions.Action
import game.CellState
import game.ShotResult

/**
  * The core data of the game. Contain all the data relative to a player.
  *
  * @param name the name of the player
  * @param score the score of the player
  * @param fistPlayer true if the player start first the game
  * @param shipsGrid the grid of ships of the player
  * @param shotsGrid the grid of the shots of the player
  * @param fleet the fleet of the player, containing its ships
  * @param action the Action set for the player
  * @param shots the previous shots of the player
  */
case class Player(
                     name: String,
                     score: Int = 0,
                     fistPlayer: Boolean = false,
                     shipsGrid: Grid = Grid(List.fill(10,10)(CellState.EMPTY)),
                     shotsGrid: Grid = Grid(List.fill(10,10)(CellState.EMPTY)),
                     fleet: List[Ship] = List(),
                     action: Action,
                     shots: List[Tuple2[Cell, ShotResult.Value]] = List()

                 ) {

    /**
      * get the state of the cell, of the shipsGrid.
      *
      * @param cell
      * @return an option value containing the state of the cell. None if cell doesn't belong to the shipsGrid.
      */
    private def getCellStateShipsGrid(cell: Cell): Option[CellState.Value] = {
        shipsGrid.getCellState(cell)
    }

    /**
      * get the state of the cell, of the shotsGrid.
      *
      * @param cell
      * @return an option value containing the state of the cell. None if cell doesn't belong to the shotsGrid.
      */
    def getCellStateShotsGrid(cell: Cell): Option[CellState.Value] = {
        shotsGrid.getCellState(cell)
    }

    /**
      * Check if all the cells of the ship are correct: the coords belong to the grid and not already occupied by a ship.
      *
      * @param ship
      * @return true if the ship is placeable, else false.
      */
    def isShipPlaceable(ship: Ship): Boolean = {
        ship.position.forall(p => shipsGrid.isCellBelongsTo(p) && this.shipsGrid.cells(p.x)(p.y).equals(CellState.EMPTY))
    }

    /**
      * Find the ship that contains the cell.
      *
      * @param cell
      * @return an option value containing the ship touched. None if no ship was touched.
      */
    private def shipTouched(cell: Cell): Option[Ship] = {
        fleet.find( x => x.isTouched(cell))
    }
    /**
      * Check if the ship is sunk.
      *
      * @param ship
      * @return true if the ship is sunk. Else false
      */
    private def isShipSunk(ship: Ship): Boolean = {
        if (fleet.contains(ship)) { ship.isSunk}
        else false
    }

    /**
      * Check if the fleet is sunk.
      *
      * @return true if the fleet is sunk.
      */
    private def isFleetSunk(): Boolean = fleet.isEmpty

    /**
      * Create a new player by incrementing the score of the player + 1.
      *
      * @return a new player with a score +1.
      */
    def incrementScore(): Player = {
        val oldScore = score
        copy(score = oldScore + 1)
    }

    /**
      * Create a new player by resseting player. Keep: name and score. If the user was not firstPlayer, he becomes it.
      *
      * @return a new player ressenting.
      */
    def ressetPlayer(): Player = {

        if (fistPlayer) {
            copy(fleet = List(), shipsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), shotsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), fistPlayer = false)
        }
        else {
            copy(fleet = List(), shipsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), shotsGrid = Grid(List.fill(10,10)(CellState.EMPTY)), fistPlayer = true)
        }
    }

    /**
      * Create a new Player with a new ship added.
      * The ship is added only if it is placeable.
      * If it is not placeable, simply return the player.
      *
      * @param ship
      * @return a new player with a ship added, if it is placeable.
      */
    def addShipToPlayer(ship: Ship): Player = {
        if (isShipPlaceable(ship)) {

            // Add the ship position on the shipsGrid.
            val newShipsGrid = shipsGrid.updateMultipleCellsState(ship.position, CellState.SHIP)

            // Add it on the fleet.
            val newFleet = ship :: fleet

            // Create a new player.
            copy(fleet = newFleet, shipsGrid = newShipsGrid)
        }
        else this
    }

    /**
      * Create a new Player without a specific ship in its fleet.
      * NOT change its grids, only fleet.
      *
      * @param ship
      * @return a new player without the ship specify in its fleet.
      */
    private def removeShipToPlayer(ship: Ship): Player = {
        val newFleet = fleet.filterNot(x => x == ship)
        copy(fleet = newFleet)
    }

    /**
      * create a new Player with one of the cell of its shipsGrid updated.
      * The cell must belongs to the shipsGrid.
      * If not, the initial player is return.
      *
      * @param cell
      * @param newState
      * @return a new Player with one of the cell of its shipsGrid updated.
      */
    private def updateShipsGrid(cell: Cell, newState: CellState.Value): Player = {
        val newGrid = shipsGrid.updateCellState(cell, newState)
        copy(shipsGrid = newGrid)
    }

    /**
      * create a new Player with one of the cell of its shotsGrid updated.
      * The cell must belongs to the shotsGrid.
      * If not, the initial player is return.
      *
      * @param cell
      * @param newState
      * @return a new Player with one of the cell of its shotsGrid updated.
      */
    private def updateShootsGrid(cell: Cell, newState: CellState.Value): Player = {
        val newGrid = shotsGrid.updateCellState(cell, newState)
        copy(shotsGrid = newGrid)

    }

    /**
      * Create a new player by removing the cell of the ship touched.
      * NOT change its grids, only affect its fleet.
      * If the ship is sunk, removed it from the fleet.
      * If no ship was touched, simply return the player.
      *
      * @param cell
      * @return a new player with the ship touched updated.
      */
    private def fleetTouched(cell: Cell): Player = {

        val shipT = shipTouched(cell)

        // If there is not ship touched.
        if (shipT.isEmpty) this
        else {
            val newShip = shipT.get.removeCell(cell)

            // If the ship is sunk, we remove it.
            if (newShip.isSunk) {
                val newFleet = fleet.filterNot( s => s == shipT.get)
                copy(fleet = newFleet)
            }
            // Else we update the fleet with the new ship.
            else {
                val newFleet = fleet.updated(fleet.indexOf(shipT.get), newShip)
                copy(fleet = newFleet)
            }
        }
    }

    /**
      * Return a new player with the opponent shoot added, and the result of the shoot.
      * Update shipsGrid.
      * Update fleet if a ship is touched.
      *
      * @param cell
      * @return a new player, and the result of the shoot.
      */
    def addOpponentShoot(cell: Cell): (Player, ShotResult.Value) = {

        val cellState = getCellStateShipsGrid(cell)

        if (cellState.isEmpty) (this, ShotResult.MISS)
        else {
            cellState.get match {
                // If there is no ship, we mark miss on our shipsGrid
                case CellState.EMPTY => (updateShipsGrid(cell, CellState.MISS), ShotResult.MISS)
                // If there is a ship, we update the fleet and the shipsGrid
                case CellState.SHIP => {
                    //update the grid
                    val playerShipsGridUpdated = updateShipsGrid(cell, CellState.TOUCH)

                    val shipT = shipTouched(cell)

                    // Not ship touched. (Normally not possible)
                    if (shipT.isEmpty) (this, ShotResult.MISS)
                    else {
                        val newShip = shipT.get.removeCell(cell)

                        // Ship touched - sunk. We remove the ship.
                        if (newShip.isSunk) {
                            val newFleet = fleet.filterNot( s => s == shipT.get)
                            val newPlayer = playerShipsGridUpdated.copy(fleet = newFleet)
                            // Fleet sunk.
                            if (newPlayer.isFleetSunk()) (newPlayer, ShotResult.FLEETSUNK)
                            // Fleet not sunk
                            else (newPlayer, ShotResult.SHIPSUNK)

                        }
                        // Ship touched - not sunk. Remove cell from ship, update shipsGrid.
                        else {
                            val newFleet = fleet.updated(fleet.indexOf(shipT.get), newShip)
                            (playerShipsGridUpdated.copy(fleet = newFleet), ShotResult.TOUCH)
                        }
                    }
                }
                // Else, cell already touched
                case _ => (this, ShotResult.ALREADYSHOOT)
            }
        }
    }

    /**
      * actualise the shotsGrid of the player according to the result of shoot.
      *
      * @param cell the cell concerned
      * @param resShoot
      * @return
      */
    def addOwnShoot(cell: Cell, resShoot: ShotResult.Value): Player = {
        val playerAddShoot = copy(shots = (cell, resShoot) :: shots)
        resShoot match {
            case ShotResult.MISS => {
                val newPlayer = playerAddShoot.updateShootsGrid(cell, CellState.MISS)
                action.displayResultShot(newPlayer, resShoot)
                newPlayer
            }
            case ShotResult.TOUCH => {
                val newPlayer = playerAddShoot.updateShootsGrid(cell, CellState.TOUCH)
                action.displayResultShot(newPlayer, resShoot)
                newPlayer
            }
            // Already touch, don't need to update the shotsGrid
            case _ => action.displayResultShot(playerAddShoot, resShoot); playerAddShoot
        }
    }

    /**
      * Create a new player by setting his fleet.
      *
      * @param descrShips : list the different names and of the ship required and its size associated
      * @return: a new player with his fleet completed.
      */
    def initialiseFleet(descrShips: List[Tuple2[String,Int]]): Player = action.initialiseFleet(this, descrShips)

    /**g
      * Shoot on a cell.
      *
      * @return the target cell
      */
    def shoot(): Cell = action.shoot(this)

}