package actions

import basis.{Cell, Player}
import game.CellState

/**
  * It defined the way to shoot for the AI Level Medium.
  */
object AI2 extends AIAction {

    /**
      * Shoot randomly on a cell, without shooting on the same cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    override def shoot(player: Player): Cell = {
        val s = getCoordinatesCell
        val cellStateShoot = player.getCellStateShotsGrid(s).get

        // if case already touched, shoot again.
        if (cellStateShoot == CellState.TOUCH || cellStateShoot == CellState.MISS) shoot(player)
        else s
    }
}
