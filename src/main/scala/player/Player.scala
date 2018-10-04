//package player
//
//import game.Utility.GridSize
//import game._
//import ship.Ship
//
//class Player(var name: String, var shipsGrid: Grid, var shotsGrid: Grid, var fleet: List[Ship] = Nil) {
//
//    /**
//      * Add a new ship on the grid if the space is available.
//      *
//      * @param ship: Ship
//      * @return Boolean. true if the ship was added, else false.
//      */
//    def addShip(ship: Ship): Boolean = {
//        if (isShipPlaceable(ship)) {
//            // add it on his fleet
//            fleet = ship :: fleet
//
//            // add it on his shipsGrid
//            ship.coord.foreach(p => this.shipsGrid.grid(p.x)(p.y) = CellState.SHIP)
//            true
//        }
//        else false
//    }
//
//    /**
//      * Check if the cell is correct: the coord belong to the grid and not already occupied by a ship.
//      *
//      * in: cell: Array[Int]
//      * out: Boolean. True if the ship was added, else False.
//      *
//      */
//    def isShipPlaceable(ship: Ship): Boolean = {
//        ship.coord.forall(p => p.x >= 0 && p.x < GridSize.value && p.y >= 0 && p.y < GridSize.value && this.shipsGrid.grid(p.x)(p.y).equals(CellState.EMPTY))
//    }
//
//    /**
//      * Return the ship touched, nil if no ship is touched
//      *
//      * @param cell: the shoot cell
//      * @return the ship touched by the cell enter or Nil if there is no ship touched
//      */
//    def shipTouched(cell: Array[Int]): Option[Ship] = ???
//
//    /**
//      * Remove the coord of the ship that was touched and pupdated the grid
//      *
//      * @param ship: the ship touched
//      * @param cell: the ship cell touched
//      * @return current player
//      */
//    def updateShipTouched(ship: Ship, cell: Array[Int]): Player = ???
//
//    /**
//      * Test if the all the ships are sunk
//      *
//      * @return true if all the ships are sunk
//      */
//    def isFleetSunk: Boolean = ???
//
//    /**
//      *
//      * @param cell: the cell
//      * @return
//      */
//    def updateCellState(cell: Array[Int], state: CellState.Value): Player = ???
//
//
//}
//
//
///*
//Que retourner quand on fait des update. Ex: updateShipTOUCHED je reoutourle le joueur? Le bateau?
// */
