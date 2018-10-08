package action

import actors.{Cell, Player}
import game.CellState

object AIAction2 extends AIAction {

    /**
      * Shoot randomly on a cell, without shooting on the same cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    def shoot(player: Player): Cell = {
        val s = getCoordinatesCell
        val cellStateShoot = player.getCellStateShootsGrid(s).get

        // if case already touched, shoot again.
        if (cellStateShoot == CellState.TOUCH || cellStateShoot == CellState.MISS) shoot(player)
        else s
    }
}
