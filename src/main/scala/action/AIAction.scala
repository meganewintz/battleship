package action

import actors.{Player, Ship}
import game.{CellState, ShotResult}
import game.Utility.Direction
import game.Utility._



abstract class AIAction extends Action {

    /**
      * Get random coordinates
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

    override def displayResultShoot(player: Player, resShoot: ShotResult.Value): Unit = return
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

    def loopMiss(player: Player, shotsList: List[Tuple2[Tuple2[Int, Int], ShotResult.Value]], loop: Int): Tuple2[Int, Int] = {
        val shot = shotsList.headOption
        if (shot.isEmpty || loop == 0) getCoordinatesCell

        else {
            val shotCell = shot.get._1
            val shotValue = shot.get._2
            shotValue match {
                case ShotResult.MISS => loopMiss(player, shotsList.tail, loop-1)
                case ShotResult.TOUCH => getCellAvailableAround(player, shotCell)
                    /*
                    getStateCellAround: List(Cell, State)
                        - if one = SHIP search nextCell(shotCell et one)
                        - else getCellAround
                     */
                case _ => getCoordinatesCell
            }
        }
    }

    override def shoot(player: Player): Tuple2[Int, Int] = {
        val prevshot0 = player.shots.headOption

        // First shot: random
        if (prevshot0.isEmpty) getCoordinatesCell
        else {

            // Previous shot SHIP SUNK: random
            if (prevshot0.get._2 == ShotResult.SHIPSUNK) getCoordinatesCell
            else {

                // Previous shot: TOUCH
                if (prevshot0.get._2 == ShotResult.TOUCH) {
                    val shotsList1 = player.shots.tail
                    val prevshot1 = shotsList1.headOption

                    // Previous shot-1: TOUCH
                    if (prevshot1.isDefined && prevshot1.get._2 == ShotResult.TOUCH) {
                        // get the next cell, check des deux sens pour savoir ou le placer au mieux
                        getNextCell(player, prevshot0.get._1, prevshot1.get._1)
                    }
                    else if(prevshot1.get._2 == ShotResult.MISS) {

                    }
                    // Previous shot-1: not TOUCH
                    else getCellAvailableAround(player, prevshot0.get._1)
                }
                // Previous shot: MISS
                else if(prevshot0.get._2 == ShotResult.MISS) {
                    loopMiss(player, player.shots.tail, 2)
                }
                else getCoordinatesCell

            }
        }
    }

    def isOutOfBound(cell: Tuple2[Int, Int]): Boolean = {
        cell._1 < 0 || cell._1 >= GridSize.value || cell._2 < 0 || cell._2 >= GridSize.value
    }

    def isAlreadyTouched(player: Player, cell: Tuple2[Int, Int]): Boolean = {
        player.getCellStateShootsGrid(cell).get != CellState.EMPTY
    }
    def getCellAvailableAround(player: Player, cell: Tuple2[Int, Int]): Tuple2[Int, Int] = {
        // on essaye c.x-1,c.y
        // -> check si: out of bound - not already touched
        //      -if true c.x+1,c.y
        val cellRight = (cell._1-1, cell._2)
        if (isOutOfBound(cellRight) || isAlreadyTouched(player: Player, cellRight)) {
            val cellLeft = (cell._1+1, cell._2)

            if (isOutOfBound(cellLeft) || isAlreadyTouched(player: Player, cellLeft)) {
                val cellUp = (cell._1, cell._2+1)

                if (isOutOfBound(cellUp) || isAlreadyTouched(player: Player, cellUp)) {
                    (cell._1, cell._2-1)
                }
                else cellUp
            }
            else cellLeft
        }
        else cellRight
    }

    // Must check each time if the cell is touched, if it is, do coord+1
    def getNextCell(player: Player, cell1: Tuple2[Int, Int], cell2: Tuple2[Int, Int]): Tuple2[Int, Int] = {
        // The 2 cells are vertically align
        if (cell1._1 == cell2._1) {

            if(cell1._2 < cell2._2) {
                val firstChoice = (cell1._1, cell1._2-1)
                if (isOutOfBound(firstChoice) || isAlreadyTouched(player, firstChoice)) (cell1._1, cell2._2+1)
                // if ship coord+1 (le faire encore 1 fois)
                else firstChoice
            }
            else {
                val firstChoice = (cell1._1, cell1._2+1)
                if (isOutOfBound(firstChoice) || isAlreadyTouched(player, firstChoice)) (cell1._1, cell2._2-1)
                else firstChoice
            }
        }
        // The ship is placed vertically
        else {

            if(cell1._1 < cell2._1) {
                val firstChoice = (cell1._1-1, cell1._2)
                if (isOutOfBound(firstChoice) || isAlreadyTouched(player, firstChoice)) (cell1._1+1, cell2._2)
                else firstChoice
            }
            else {
                val firstChoice = (cell1._1+1, cell1._2)
                if (isOutOfBound(firstChoice) || isAlreadyTouched(player, firstChoice)) (cell1._1-1, cell2._2)
                else firstChoice
            }
        }
    }
}


/*
Case previous shot:
- shot empty
    - Random

- shot SHIP FLEET
    - Random

- shot TOUCH: check until 4 previous shots.
    - shot-1 TOUCH -> checkCommunCoordxOrY then checkCaseAvailableAround

    - shot-1 empty -> checkCaseAvailableAround(shot) (gauche - droite - haut - bas)
    - shot-1 MISS -> checkCaseAvailableAround(shot) (gauche - droite - haut - bas)

- shot MISS
    - shot-1 empty -> (s-1,s) -> random
    - shot-1 MISS ->
        - shot-2 empty -> random
        - shot-2 MISS
            - shot-3 empty -> random
            - shot-3 MISS -> random
            - shot-3 TOUCH -> checkCaseAvailableAround(shot-3) (gauche - droite - haut - bas)
        - shot-2 TOUCH -> checkCaseAvailableAround(shot-2) (gauche - droite - haut - bas)
    - shot-1 TOUCH -> checkCaseAvailableAround(shot-1) (gauche - droite - haut - bas)
        - ship-2 TOUCH
    - shot-1 MISS ->
        - shot-2 empty

each time: checkavalable
    - out of bounds -> other side
    - alreadyshoot -> other side



- ALREADYSHOOT
 */