package actions

import basis._
import game.ShotResult
import game.Utility._

import scala.io.StdIn.readLine

/**
  * It defined the way to play for an user.
  */
object HumanAction extends Action {

    /**
      * The coordinate choose by the user
      * He can enter: A1
      * The result looks like: (0,0)
      *
      * @return a cell corresponding to the coordinates choose by the user.
      */
    override def getCoordinatesCell: Cell = {
        showPromptCell()
        val userInput = getUserInput() // ex received : A1

        if (userInput.length < 2) {
            showInvalidCoordMessage; getCoordinatesCell
        }
        else {
            val coord0 = userInput.head.toString
            val coord1 = userInput.tail

            if (mapLetterCoord.contains(coord0) && mapNumberCoord.contains(coord1)) {
                return Cell(mapNumberCoord.apply(coord1), mapLetterCoord.apply(coord0))
            }
            else showInvalidCoordMessage;
            getCoordinatesCell
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
      * It's an user: we ask to the user to place each ship.
      *
      * @param player the player concerned
      * @param descrShips list the different names and of the ship required and its size associated
      * @return a new player with his fleet completed.
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

            // If the ship is placeabled, we add it to the player and go to the next ship
            if (player.isShipPlaceable(ship)) {
                val newBoard = player.addShipToPlayer(ship)
                clear
                initialiseFleet(newBoard, descrShips.tail)
            }
            // If the ship is not placeabled, try again
            else {showInvalidPlacementMessage; initialiseFleet(player, descrShips)}
        }
    }

    /**
      * The user shoot on a cell.
      *
      * @param player the shooter
      * @return the target cell
      */
    override def shoot(player: Player): Cell = {
        println(player.shipsGrid + "\n")
        println(player.shotsGrid)
        showPlayerTour(player)
        getCoordinatesCell
    }


    /**
      * Display the result of the shot: his grids and the message displaying the result.
      *
      * @param player the shooter
      * @param resShot the result of his shot
      */
    override def displayResultShot(player: Player, resShot: ShotResult.Value ): Unit = {
        clear
        println(player.shipsGrid + "\n")
        println(player.shotsGrid)
        println(resShot)
        showContinueMessage
        readLine()
    }
}