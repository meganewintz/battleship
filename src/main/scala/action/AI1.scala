package action

import actors.{Cell, Player}

object AIAction1 extends AIAction {

    /**
      * Shoot randomly on a cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    def shoot(player: Player): Cell = {
        getCoordinatesCell
    }
}