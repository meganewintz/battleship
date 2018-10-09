package actions

import basis._
import game.{CellState, ShotResult}
import game.Utility.Direction
import game.Utility._



trait AIAction extends Action {

    /**
      * Get random coordinates
      * He can enter: A1
      * The result looks like: (0,0)
      *
      * @return a Tuple2 containing the coordinates choose by the user.
      */
    override def getCoordinatesCell: Cell = {
        val r = scala.util.Random
       Cell(r.nextInt(GridSize.value),r.nextInt(GridSize.value))
    }


    /**
      * Create a new player by setting his fleet.
      * It's an AI: ships are placed randomly.
      *
      * @param player the player concerned
      * @param descrShips list the different names and of the ship required and its size associated
      * @return a new player with his fleet completed.
      */
    override def initialiseFleet(player: Player, descrShips: List[Tuple2[String,Int]]): Player = {

        // if descrShips is empty, all ships were successfully added
        if (descrShips.isEmpty) player

        else {
            val firstCellShip = getCoordinatesCell

            val r = scala.util.Random
            val direction = Array(Direction.HORIZONTAL, Direction.VERTICAL)
            val directionShip = direction(r.nextInt(2))

            // We are sure that the cell, direction and description are correct
            val ship = Ship(firstCellShip, directionShip, descrShips.head).get

            // If the ship is placeabled, we add it to the player and go to the next ship
            if (player.isShipPlaceable(ship)) {
                val newBoard = player.addShipToPlayer(ship)
                initialiseFleet(newBoard, descrShips.tail)
            }
            // If the ship is not placeabled, try again
            else {
                initialiseFleet(player, descrShips)
            }
        }
    }
    /**
      * Shoot on a cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    def shoot(player: Player): Cell

    /**
      * It's an AI. No need to display its result. Do nothing.
      *
      * @param player the shooter
      * @param resShot the result of his shot
      */
    override def displayResultShot(player: Player, resShot: ShotResult.Value): Unit = return
}