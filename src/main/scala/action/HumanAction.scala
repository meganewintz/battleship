package action

import actors._
import game.ShootResult
import game.Utility._

import scala.io.StdIn.readLine

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
            showPlaceShipMessage(player, descrShips.head._1, descrShips.head._2)

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


    override def displayResultShoot(player: Player,resShoot: ShootResult.Value ): Unit = {
        clear
        println(player.shipsGrid + "\n")
        println(player.shootsGrid)
        println(resShoot)
        showContinueMessage
        readLine()
    }
}