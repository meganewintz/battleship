package action

import actors._
import game.Utility._
import game._

import scala.io.StdIn.readLine

trait Action {


    /**
      * Return a Tuple2 corresponding of the cell choose by the player.
      *
      * @param grid: the target grid
      * @return a Tuple2 containing the coordinates choose by the player.
      */
    def getCoordinatesCell: Cell

    /**
      * Create a new player by setting his fleet.
      *
      * @param player      : the player concerned
      * @param descrShips : list the different names and of the ship required and its size associated
      * @return: a new player with his fleet completed.
      */
    def initialiseFleet(player: Player, descrShips: List[Tuple2[String, Int]]): Player

    /**
      * Shoot on a cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    def shoot(player: Player): Cell

    /**
      * Display the result of the shot.
      *
      * @param player the shooter
      * @param resShot the result of his shot
      */
    def displayResultShot(player: Player, resShot: ShotResult.Value): Unit
}

