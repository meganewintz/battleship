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
    def getCoordinatesCell: Tuple2[Int, Int]

    /**
      * Create a new player by setting his fleet.
      *
      * @param board      : the board concerned
      * @param descrShips : list the different names and of the ship required and its size associated
      * @return: a new player with his fleet completed.
      */
    def initialiseFleet(player: Player, descrShips: List[Tuple2[String, Int]]): Player

    def shoot(player: Player): Tuple2[Int, Int]

    def displayResultShoot(player: Player, resShoot: ShotResult.Value): Unit
}


/*
- name: String
- score: Int
- firstPlayer: Boolean = false,
- board: Board
    - shipsGrid: Grid = Grid(List.fill(10,10)(CellState.EMPTY)),
    - shootsGrid: Grid = Grid(List.fill(10,10)(CellState.EMPTY)),
    - fleet: List[Ship] = List()

Methods:
- createFleet
- getCoordinates
- getDirectionShip
- playAgain?

Difference Human AI:
- getCoord
- placeShips
- affichage des res : on ne voit jamais la grille de AI.


un tir:

HUMAN
J1 affiche sa grille -> Que H           OK
J1 entre le tir -> shoot: Tuple2 Diff   OK
J2 ajoute le tir -> addShotOpponent: Player, res (MISS, TOUCH, COULE, FLOTTECOULLEE) Commun
J1 ajoute le tir -> addMyShot(cell, res): Player Commun
affichage le res: Que H
    - res du tir
    - J1 nouvelles grilles
J1 appuye sur entrer mon continuer Que H
*/

