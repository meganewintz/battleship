package action

import actors.{Cell, Player}
import game.{CellState, ShotResult}
import game.Utility.GridSize

object AIAction3 extends AIAction {

    /**
      * Shoot smartly on a cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    override def shoot(player: Player): Cell = {
        val prevshot0 = player.shots.headOption

        // There is NOT previous shot: random
        if (prevshot0.isEmpty) shootRandomCellNotTouched(player)
        else {

            // Previous shot SHIP SUNK: random
            if (prevshot0.get._2 == ShotResult.SHIPSUNK) shootRandomCellNotTouched(player)
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
                    else {

                        val cellTouchAround = getCellTouchedAround(player, prevshot0.get._1)

                        // If there is another cell touched around, getNextCell
                        if (cellTouchAround.isDefined) {
                            getNextCell(player, prevshot0.get._1, cellTouchAround.get)
                        }

                        // Else, shot a cell around the prev shot
                        else {
                            getCellAvailableAround(player, prevshot0.get._1)
                        }
                    }
                }

                // Previous shot: MISS
                else if(prevshot0.get._2 == ShotResult.MISS) {
                    val prevCellShot = searchCellTouched(player, player.shots.tail, 3)

                    // If there is a previous shot touch
                    if (prevCellShot.isDefined) {
                        val cellTouchAround = getCellTouchedAround(player, prevCellShot.get)

                        // If there is another cell touched around, getNextCell
                        if (cellTouchAround.isDefined) {
                            getNextCell(player, prevCellShot.get, cellTouchAround.get)
                        }

                        // Else, shot a cell around the prev shot
                        else {
                            getCellAvailableAround(player, prevCellShot.get)
                        }
                    }
                    else shootRandomCellNotTouched(player)
                }
                else shootRandomCellNotTouched(player)
            }
        }
    }

    /**
      * Shoot randomly on a cell, without shooting on the same cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    def shootRandomCellNotTouched(player: Player): Cell = {
        val s = getCoordinatesCell
        val cellStateShoot = player.getCellStateShootsGrid(s).get

        // if case already touched, shoot again.
        if (cellStateShoot == CellState.TOUCH || cellStateShoot == CellState.MISS) shoot(player)
        else s
    }

    /**
      * Search a cell touched on the n last shots of the player.
      * None if there is no touched cell
      *
      * @param player the player which we test if his cell is touched.
      * @param shotsList
      * @param n the number of previous shots which we search a cell touched.
      * @return an Option[Cell] containing the cell touched. None if there is no cell touched.
      */
    def searchCellTouched(player: Player, shotsList: List[Tuple2[Cell, ShotResult.Value]], n: Int): Option[Cell] = {
        val shot = shotsList.headOption
        if (shot.isEmpty || n == 0) None

        else {
            val shotCell = shot.get._1
            val shotValue = shot.get._2

            if (shotValue == ShotResult.TOUCH) Some(shotCell)
            else searchCellTouched(player, shotsList.tail, n - 1)
        }
    }

    /**
      * Search a cell touched around a specific cell.
      *
      * @param player the player which we test if his cell is touched.
      * @param cell
      * @return an Option[Cell] containing the cell touched. None if there is no cell touched.
      */
    def getCellTouchedAround(player: Player, cell: Cell): Option[Cell] = {
        val cellRight = Cell(cell.x, cell.y-1)
        if(isCellTouched(player, cellRight))  return Some(cellRight)

        val cellLeft = Cell(cell.x, cell.y+1)
        if(isCellTouched(player, cellLeft))  return Some(cellLeft)

        val cellUp = Cell(cell.x-1, cell.y)
        if(isCellTouched(player, cellUp))  return Some(cellUp)

        val cellDown = Cell(cell.x+1, cell.y)
        if(isCellTouched(player, cellDown))  Some(cellDown)

        else None
    }

    /**
      * Test if the player has already shot this cell and if its value is TOUCH.
      * @param player the player which we test if his cell is touched.
      * @param cell
      * @return true if the specific cell is TOUCH in the shootsGrid of the player.
      */
    def isCellTouched(player: Player, cell: Cell): Boolean = {
        val cellState = player.getCellStateShootsGrid(cell)
        cellState.isDefined && cellState.get == CellState.TOUCH
    }

    /**
      * Test if the cell is out of bound.
      * @param cell
      * @return true if the cell is out of bound.
      */
    def isOutOfBound(cell: Cell): Boolean = {
        cell.x < 0 || cell.x >= GridSize.value || cell.y < 0 || cell.y >= GridSize.value
    }

    /**
      * Test if the player already shot this cell.
      *
      * @param player the player which we test if his cell is touched.
      * @param cell
      * @return true if the user already shot this cell.
      */
    def isAlreadyShot(player: Player, cell: Cell): Boolean = {
        player.getCellStateShootsGrid(cell).get != CellState.EMPTY
    }

    /**
      * Search a cell not already shot by the player around a specific cell.
      *
      * @param player
      * @param cell
      * @return an Option[Cell] containing the cell available. None if there is no cell available.
      */
    def getCellAvailableAround(player: Player, cell: Cell): Cell = {
        // on essaye c.x-1,c.y
        // -> check si: out of bound - not already touched
        //      -if true c.x+1,c.y
        val cellRight = nextRight(cell)
        if (isOutOfBound(cellRight) || isAlreadyShot(player: Player, cellRight)) {
            val cellLeft = nextLeft(cell)

            if (isOutOfBound(cellLeft) || isAlreadyShot(player: Player, cellLeft)) {
                val cellUp = nextUp(cell)

                if (isOutOfBound(cellUp) || isAlreadyShot(player: Player, cellUp)) {
                    nextDown(cell)
                }
                else cellUp
            }
            else cellLeft
        }
        else cellRight
    }



    /**
      * Return the next cell available on the left of the 2 cells (the player does not already shot it). None if there is not cell available.
      * Required: the two cells must be aligned horizontally.
      *
      * @param player
      * @param cell1
      * @param cell2
      * @return an Option[Cell] containing the cell available. None if there is no cell available.
      */
    def nextLeftAvailable(player: Player, cell1: Cell, cell2: Cell): Option[Cell] = {
        val cellsOrdered = orderCells(cell1, cell2)
        val firstCell = cellsOrdered.head
        val secondCell = cellsOrdered.last

        val nextCell = nextLeft(firstCell)

        if (isOutOfBound(nextCell)) None
        else if (isAlreadyShot(player, nextCell)) {
            // on check si la case suivante gauche 2 fois
            val nextCell2 = nextLeft(nextCell)

            if (isOutOfBound(nextCell2)) None
            else if (isAlreadyShot(player, nextCell2)) {

                val nextCell3 = nextLeft(nextCell2)

                if (isOutOfBound(nextCell3) || isAlreadyShot(player, nextCell3)) None
                else Some(nextCell3)
            }
            else Some(nextCell2)
        }
        // The nextCell is available
        else Some(nextCell)
    }

    /**
      * Return the next cell available on the right of the 2 cells (the player does not already shot it). None if there is not cell available.
      * Required: the two cells must be aligned horizontally.
      *
      * @param player
      * @param cell1
      * @param cell2
      * @return an Option[Cell] containing the cell available. None if there is no cell available.
      */
    def nextRightAvailable(player: Player, cell1: Cell, cell2: Cell): Option[Cell] = {
        val cellsOrdered = orderCells(cell1, cell2)
        val firstCell = cellsOrdered.head
        val secondCell = cellsOrdered.last

        val nextCell= nextRight(secondCell)

        if (isOutOfBound(nextCell)) None
        else if (isAlreadyShot(player, nextCell)) {
            // on check si la case suivante gauche 2 fois
            val nextCell2 = nextRight(nextCell)

            if (isOutOfBound(nextCell2)) None
            else if (isAlreadyShot(player, nextCell2)) {

                val nextCell3 = nextRight(nextCell2)

                if (isOutOfBound(nextCell3) || isAlreadyShot(player, nextCell3)) None
                else Some(nextCell3)
            }
            else Some(nextCell2)
        }
        // The nextCell is available
        else Some(nextCell)
    }

    /**
      * Return the next cell available on the top of the 2 cells (the player does not already shot it). None if there is not cell available.
      * Required: the two cells must be aligned vertically.
      *
      * @param player
      * @param cell1
      * @param cell2
      * @return an Option[Cell] containing the cell available. None if there is no cell available.
      */
    def nextUpAvailable(player: Player, cell1: Cell, cell2: Cell): Option[Cell] = {
        val cellsOrdered = orderCells(cell1, cell2)
        val firstCell = cellsOrdered.head
        val secondCell = cellsOrdered.last

        val nextCell= nextUp(secondCell)

        if (isOutOfBound(nextCell)) None
        else if (isAlreadyShot(player, nextCell)) {
            // on check si la case suivante gauche 2 fois
            val nextCell2 = nextUp(nextCell)

            if (isOutOfBound(nextCell2)) None
            else if (isAlreadyShot(player, nextCell2)) {

                val nextCell3 = nextUp(nextCell2)

                if (isOutOfBound(nextCell3) || isAlreadyShot(player, nextCell3)) None
                else Some(nextCell3)
            }
            else Some(nextCell2)
        }
        // The nextCell is available
        else Some(nextCell)
    }

    /**
      * Return the next cell available on the bottom of the 2 cells (the player does not already shot it). None if there is not cell available.
      * Required: the two cells must be aligned vertically.
      *
      * @param player
      * @param cell1
      * @param cell2
      * @return an Option[Cell] containing the cell available. None if there is no cell available.
      */
    def nextDownAvailable(player: Player, cell1: Cell, cell2: Cell): Option[Cell] = {
        val cellsOrdered = orderCells(cell1, cell2)
        val firstCell = cellsOrdered.head
        val secondCell = cellsOrdered.last

        val nextCell= nextDown(secondCell)

        if (isOutOfBound(nextCell)) None
        else if (isAlreadyShot(player, nextCell)) {
            // on check si la case suivante gauche 2 fois
            val nextCell2 = nextDown(nextCell)

            if (isOutOfBound(nextCell2)) None
            else if (isAlreadyShot(player, nextCell2)) {

                val nextCell3 = nextDown(nextCell2)

                if (isOutOfBound(nextCell3) || isAlreadyShot(player, nextCell3)) None
                else Some(nextCell3)
            }
            else Some(nextCell2)
        }
        // The nextCell is available
        else Some(nextCell)
    }

    def nextLeft(cell: Cell): Cell = Cell(cell.x, cell.y-1)
    def nextRight(cell: Cell): Cell = Cell(cell.x, cell.y+1)
    def nextUp(cell: Cell): Cell = Cell(cell.x-1, cell.y)
    def nextDown(cell: Cell): Cell = Cell(cell.x+1, cell.y)

    // Must check each time if the cell is touched, if it is, do coord+1
    // The two cell are order

    def orderCells(cell1: Cell, cell2: Cell): List[Cell] = {
        if (cell1.x < cell2.x || cell1.y < cell2.y) {
            List(cell1, cell2)
        }
        else {
            List(cell2, cell1)
        }
    }

    /**
      * Get the next cell avalaible on the row/col of the two cells. If there is not, get a cell randomly
      * Required: the cells must be aligned vertically or horizontally.
      *
      * @param player
      * @param firstCell
      * @param secondCell
      * @return an Option[Cell] containing the cell available. None if there is no cell available.
      */
    def getNextCell(player: Player, firstCell: Cell, secondCell: Cell): Cell = {

        // The 2 cells are horizontally align.
        if (firstCell.x == secondCell.x) {

            val nextLeftAvai = nextLeftAvailable(player, firstCell, secondCell)
            val nextRightAvai = nextRightAvailable(player, firstCell, secondCell)

            if (nextLeftAvai.isDefined) nextLeftAvai.get
            else if(nextRightAvai.isDefined) nextRightAvai.get
            else shootRandomCellNotTouched(player)
        }
        // The 2 cells are vertically align.
        else {
            val nextUpAvai = nextUpAvailable(player, firstCell, secondCell)
            val nextDownAvai = nextDownAvailable(player, firstCell, secondCell)
            if (nextUpAvai.isDefined) nextUpAvai.get
            else if (nextDownAvai.isDefined) nextDownAvai.get
            else shootRandomCellNotTouched(player)
        }
    }
}
