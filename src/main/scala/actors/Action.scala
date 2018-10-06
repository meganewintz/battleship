package actors

import game._
import game.Utility._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

trait Action {


    /**
      * Return a Tuple2 corresponding of the cell choose by the player.
      *
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

    def displayResultShoot(player: Player, resShoot: ShootResult.Value): Unit
}

object HumanAction extends Action {

    /**
      * The coordinate choose by the user
      * He can enter: A1
      * The result looks like: (0,0)
      *
      * @return a Tuple2 containing the coordinates choose by the user.
      */
    override def getCoordinatesCell: Tuple2[Int, Int] = {
        showPromptCell()
        val userInput = getUserInput() // ex received : A1

        if (userInput.length != 2) {
            showInvalidCoordMessage; getCoordinatesCell
        }
        else {
            val coord = userInput.split("") // ex: ("A", "1")

            if (mapLetterCoord.contains(coord(0)) && mapNumberCoord.contains(coord(1))) {
                return (mapNumberCoord.apply(coord(1)), mapLetterCoord.apply(coord(0)))
            }
            else showInvalidCoordMessage; getCoordinatesCell
        }
    }

    override def displayResultShoot(player: Player,resShoot: ShootResult.Value ): Unit = {
        clear
        println(player.shipsGrid + "\n")
        println(player.shootsGrid)
        println(resShoot)
        showContinueMessage
        readLine()
    }

    /**
      * The direction of the ship choose by the user.
      * Direction.HORIZONTAL or Direction.VERTICAL
      *
      * @return a string containing the direction enter by the user
      */
    def getDirectionShip: String = {
        showPromptDirectionShip()
        val userInput = getUserInput()

        userInput match {
            case Direction.HORIZONTAL | Direction.VERTICAL => return userInput
            case _ => showInvaliDirectionMessage; getDirectionShip
        }
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
        if (descrShips.isEmpty) {clear; player}

        else {
            print(player.shipsGrid)
            showPlaceShipMessage(descrShips.head._1, descrShips.head._2)

            val firstCellShip = getCoordinatesCell
            val directionShip = getDirectionShip

            // We are sure that the cell, direction and description are correct
            val ship = Ship(firstCellShip, directionShip, descrShips.head).get

            // If the ship is placeabled, we add it to the board and go to the next ship
            if (player.isShipPlaceable(ship)) {
                val newBoard = player.addShipToPlayer(ship)
                clear
                initialiseFleet(newBoard, descrShips.tail)
            }
            // If the ship is not placeabled, try again
            else {showInvalidPlacementMessage; initialiseFleet(player, descrShips)}
        }
    }

    def shoot(player: Player): Tuple2[Int, Int] = {
        println(player.shipsGrid + "\n")
        println(player.shootsGrid)
        showPlayerTour(player)
        getCoordinatesCell
    }
}

object AIAction extends Action {

    /**
      * The coordinate choose by the user
      * He can enter: A1
      * The result looks like: (0,0)
      *
      * @return a Tuple2 containing the coordinates choose by the user.
      */
    override def getCoordinatesCell: Tuple2[Int, Int] = {
        val r = scala.util.Random
        (r.nextInt(9),r.nextInt(9))
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

    override def shoot(player: Player): Tuple2[Int, Int] = {
        getCoordinatesCell
    }

    override def displayResultShoot(player: Player, resShoot: ShootResult.Value): Unit = clear
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

