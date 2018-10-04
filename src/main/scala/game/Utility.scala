package game


import actors._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.StdIn.readChar
import scala.sys.process._

object CellState extends Enumeration {
    val EMPTY = Value("-")
    val MISS = Value("x")
    val TOUCH = Value("o")
    val SHIP = Value("s")
}

object Utility {

    /**
      * The correspondence between the string letter coordinates and the coordinates store.
      *
      * @return a Map object
      */
    def mapLetterCoord =
        Map("A" -> 0,
            "B" -> 1,
            "C" -> 2,
            "D" -> 3,
            "E" -> 4,
            "F" -> 5,
            "G" -> 6,
            "H" -> 7,
            "I" -> 8,
            "J" -> 9
        )

    /**
      * The correspondence between the string number coordinates and the coordinates store.
      *
      * @return a Map object
      */
    def mapNumberCoord =
        Map("1" -> 0,
            "2" -> 1,
            "3" -> 2,
            "4" -> 3,
            "5" -> 4,
            "6" -> 5,
            "7" -> 6,
            "8" -> 7,
            "9" -> 8,
            "10" -> 9
        )

    /**
      * The size of our grids
      */
    object GridSize {
        val value = 10
    }

    /**
      * The ship direction available.
      */
    object Direction {
        val HORIZONTAL = "H"
        val VERTICAL = "V"
    }

    /**
      * PlayerAgain object
      */
    object PlayAgain {
        val YES = "Y"
        val NO = "N"
    }

    /**
      * The ships list for the game: name and size.
      */
    val descrShips = List(("Carrier", 5), ("Battleship", 4), ("Cruiser", 3), ("Submarine", 3), ("Destroyer", 2))


    /**
      * To clear the termial
      */
    def clear = println("clear".!)

    def showPlayerTour(player: Player): Unit = print("\n" + player.name + " it's your turn.\n")

    def showPlaceShipMessage(name: String, sizeShip: Int): Unit = print("\n\nPlace the "+ name + " of size " + sizeShip +"\n")

    def showPromptStartCellShip(): Unit = print("\nEnter the cell of the ship. Ex: A1 :\n")

    def showPromptDirectionShip(): Unit = print("\nEnter the direction of the ship (H)orizontal or (V)ertical :\n")

    def showPromptCell(): Unit = print("\nEnter the target cell. Ex: A1 :\n")

    def showInvaliDirectionMessage: Unit = print("\nThe direction enter is invalid. \n")

    def showInvalidPlacementMessage: Unit = print("\n /!\\ Your ship is out of bouds or there is already a ship placed. Try again. \n")

    def showInvalidCoordMessage: Unit = print("\nThe coordinates are invalid. Try again.\n")

    def showAlreadyShotMessage: Unit = print("\nYou have already shot this cell.\n")

    def showMissShotMessage: Unit = print("\nThere is nothing here. You miss your target!\n")

    def showTouchShipMessage: Unit = print("\nYou touched a ship!\n")

    def showSunkShipMessage: Unit = print("\nA ship is sunk.\n")

    def showSunkFleetMessage: Unit = print("\nAll the ships were sunk. You win the party!\n")

    def showPlayAgainMessage: Unit = print("\nDo you want to play again? (Y)es/(N)o.\n")

    def showInvalidAnswer: Unit = print("\nInvalid answer. Try again.\n")

    def getUserInput(): String = readLine.trim.toUpperCase


    /**
      * The coordinate choose by the user
      * He can enter: A1
      * The result looks like: (0,0)
      *
      * @return a Tuple2 containing the coordinates choose by the user.
      */
    @tailrec
    def getCoordinatesCellUser: Tuple2[Int, Int] = {
        showPromptCell()
        val userInput = getUserInput() // ex received : A1

        if (userInput.length != 2) {
            showInvalidCoordMessage; getCoordinatesCellUser
        }
        else {
            val coord = userInput.split("") // ex: ("A", "1")

            if (mapLetterCoord.contains(coord(0)) && mapNumberCoord.contains(coord(1))) {
                return (mapNumberCoord.apply(coord(1)), mapLetterCoord.apply(coord(0)))
            }
            else showInvalidCoordMessage; getCoordinatesCellUser
        }
    }

    /**
      * The direction of the ship choose by the user.
      * Direction.HORIZONTAL or Direction.VERTICAL
      *
      * @return a string containing the direction enter by the user
      */
    @tailrec
    def getDirectionShipUser: String = {
        showPromptDirectionShip()
        val userInput = getUserInput()

        userInput match {
            case Direction.HORIZONTAL | Direction.VERTICAL => return userInput
            case _ => showInvaliDirectionMessage; getDirectionShipUser
        }
    }

    /**
      * ask to the user if he wants to play again.
      *
      * @return true if the user want to play again
      */
    @tailrec
    def getPlayAgainActionUser: Boolean = {
        showPlayAgainMessage
        val userInput = getUserInput()

        userInput match {
            case PlayAgain.YES =>  true
            case PlayAgain.NO => false
            case _ => showInvalidAnswer; getPlayAgainActionUser
        }
    }

    /**
      * Create a new player by setting his fleet.
      *
      * @param player: the player concerned
      * @param descrShips: list the different names and of the ship required and its size associated
      * @return: a new player with his fleet completed.
      */
    @tailrec
    def createFleetPlayer(player: Player, descrShips: List[Tuple2[String,Int]]): Player = {

        // if descrShips is empty, all ships were successfully added
        if (descrShips.isEmpty) return player

        else {
            print(player.shipsGrid)
            showPlaceShipMessage(descrShips.head._1, descrShips.head._2)

            val firstCellShip = getCoordinatesCellUser
            val directionShip = getDirectionShipUser

            // We are sure that the cell, direction and description are correct
            val ship = createShip(firstCellShip, directionShip, descrShips.head).get

            // If the ship is placeabled, we add it to the player and go to the next ship
            if (player.isShipPlaceable(ship)) {
                val newPlayer = PlayerUtil.addShipToPlayer(player, ship)
                clear
                createFleetPlayer(newPlayer, descrShips.tail)
            }
            // If the ship is not placeabled, try again
            else {showInvalidPlacementMessage; createFleetPlayer(player, descrShips)}

        }
    }

    // Peut etre a position dans le compagnon de Ship
    /**
      * Create a ship according to its initial cell, its direction and its description
      *
      * @param firstCell
      * @param direction
      * @param descrShip
      * @return a ship
      */
    def createShip(firstCell: Tuple2[Int,Int], direction: String, descrShip: Tuple2[String,Int]): Option[Ship] = {

        if ( direction != Direction.HORIZONTAL && direction != Direction.VERTICAL) None
        else {
            val position: List[Tuple2[Int, Int]] = {
                val size = descrShip._2
                direction match {
                    case Direction.HORIZONTAL => List.iterate(firstCell, size)(cell => (cell._1, cell._2 + 1))
                    case Direction.VERTICAL => List.iterate(firstCell, size)(cell => (cell._1 + 1, cell._2))
                }
            }
            Some(Ship(descrShip._1, position.toSet))
        }
    }

    /**
      * Loop of the shoots player. The loop finish when a player win, the adverse fleet is sunk.
      *
      * @param activePlayer the active player
      * @param passivePlayer the passive player
      * @return (Player,Player) the first player represent the winner and the second the looser
      */
    def shootsLoop(activePlayer: Player, passivePlayer: Player): Tuple2[Player,Player] = {

        // Ask the shoot cell to the active player
        print(activePlayer.shipsGrid)
        print(activePlayer.shootsGrid)
        showPlayerTour(activePlayer)
        val cell = getCoordinatesCellUser

        // Take the state of the cell shoot
        val cellState = passivePlayer.getCellStateShipsGrid(cell).get
        println("cell state: " + cellState + "\n")

        cellState match {

            case CellState.EMPTY    => {
                val newActivePlayer = PlayerUtil.updateShootsGrid(activePlayer, cell, CellState.MISS)
                val newPassivePlayer = PlayerUtil.addAdverseShoot(passivePlayer, cell)
                showMissShotMessage
                shootsLoop(newPassivePlayer, newActivePlayer)
            }
            // If there is a ship, we update the fleet and the shipsGrid of the passive player
            case CellState.SHIP     => {
                val shipTouch = passivePlayer.shipTouched(cell).get
                val newActivePlayer = PlayerUtil.updateShootsGrid(activePlayer, cell, CellState.TOUCH)
                val newPassivePlayer = PlayerUtil.addAdverseShoot(passivePlayer, cell)
                showTouchShipMessage

                // check if the ship sie is 1 and it was touched, we will be sunk.
                if (shipTouch.size == 1) {
                    showSunkShipMessage
                    // check if the fleet is sunk ==> active player win
                    if (passivePlayer.isFleetSunk()) {
                        showSunkFleetMessage
                        // increamente the score of the winner
                        val activePlayerWinner = PlayerUtil.incrementScore(activePlayer)
                        return (activePlayerWinner, newPassivePlayer)
                    }
                    else {
                        println("Press any key to continue")
                        readChar()
                        shootsLoop(newPassivePlayer, newActivePlayer)
                    }
                }
                else {
                    println("Press any key to continue")
                    readChar()
                    shootsLoop(newPassivePlayer, newActivePlayer)
                }
            }
            // Else, cell already shot
            case _                  => {
                showAlreadyShotMessage
                println("Press any key to continue")
                readChar()
                shootsLoop(passivePlayer, activePlayer)
            }
        }
    }

}

/*
1.  add 5 ships j1
    add 5 ships j2
2.  j1 play:
        display shipsGrod
        display shootGrid
        enter coord
        check coord on the shipsGrid j2
        - coord = TOUCH or MISS --> already touched, try again
        - coord = EMPTY --> update StateCell(MISS)
        - coord = SHIP
                    - updateStateCell(TOUCH)
                    - delete cell ship touched
                    - check if the ship is sunk
                        - check if the party is finished
    j2 play
3. display j winner
 */