package actions

import basis.{Cell, Player}

/**
  * It defined the way to shoot for the AI Level Beginner.
  */
object AI1 extends AIAction {

    /**
      * Shoot randomly on a cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    override def shoot(player: Player): Cell = {
        getCoordinatesCell
    }
}