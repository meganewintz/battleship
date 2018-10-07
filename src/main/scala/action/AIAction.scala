package action

import actors.{Player, Ship}
import game.{CellState, ShootResult}
import game.Utility.Direction
import game.Utility._



abstract class AIAction extends Action {

    /**
      * The coordinate choose by the user
      * He can enter: A1
      * The result looks like: (0,0)
      *
      * @return a Tuple2 containing the coordinates choose by the user.
      */
    override def getCoordinatesCell: Tuple2[Int, Int] = {
        val r = scala.util.Random
        (r.nextInt(GridSize.value),r.nextInt(GridSize.value))
    }


    /**
      * Create a new player by setting his fleet.
      *
      * @param board: the board concerned
      * @param descrShips: list the different names and of the ship required and its size associated
      * @return: a new player with his fleet completed.
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

            // If the ship is placeabled, we add it to the board and go to the next ship
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

    def shoot(player: Player): Tuple2[Int, Int]

    override def displayResultShoot(player: Player, resShoot: ShootResult.Value): Unit = return
}

object AIAction1 extends AIAction {

    def shoot(player: Player): Tuple2[Int, Int] = {
        getCoordinatesCell
    }
}

object AIAction2 extends AIAction {

    def shoot(player: Player): Tuple2[Int, Int] = {
        val s = getCoordinatesCell
        val cellStateShoot = player.getCellStateShootsGrid(s).get

        // if case already touched, shoot again.
        if (cellStateShoot == CellState.TOUCH || cellStateShoot == CellState.MISS) shoot(player)
        else s
    }
}

object AIAction3 extends AIAction {
    def shoot(player: Player): Tuple2[Int, Int] = {
        getCoordinatesCell
    }
}