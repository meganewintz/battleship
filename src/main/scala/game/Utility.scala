package game


import actors._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.sys.process._

object CellState extends Enumeration {
    val EMPTY = Value("-")
    val MISS = Value("X")
    val TOUCH = Value("O")
    val SHIP = Value("s")
}

/**
  * The shoot results available.
  */
object ShootResult extends Enumeration {
    val MISS = Value("There is nothing here. You MISS your target!")
    val TOUCH = Value("You TOUCHED a ship!")
    val SHIPSUNK = Value("A SHIP is SUNK!")
    val FLEETSUNK = Value("All the SHIPS were SUNK!")
    val ALREADYSHOOT = Value("You have ALREADY SHOOT this cell.")
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
      * To clear the termiNal
      */
    def clear = println("clear".!)

    def showPlayerTour(player: Player): Unit = print("\n" + player.name + " it's your turn.\n")

    def showPlayerWinnerMessage(player: Player): Unit = print("\n" + player.name + " win the game!\n")

    def showPlaceShipMessage(name: String, sizeShip: Int): Unit = print("\n\nPlace the "+ name + " of size " + sizeShip +"\n")

    def showPromptStartCellShip(): Unit = print("\nEnter the cell of the ship. Ex: A1 :\n")

    def showPromptDirectionShip(): Unit = print("\nEnter the direction of the ship (H)orizontal or (V)ertical :\n")

    def showPromptCell(): Unit = print("\nEnter the target cell. Ex: A1 :\n")

    def showInvaliDirectionMessage: Unit = print("\nThe direction enter is invalid. \n")

    def showInvalidPlacementMessage: Unit = print("\n /!\\ Your ship is out of bouds or there is already a ship placed. Try again. \n")

    def showInvalidCoordMessage: Unit = print("\nThe coordinates are invalid. Try again.\n")

    def showPlayAgainMessage: Unit = print("\nDo you want to play again? (Y)es/(N)o.\n")

    def showChoiceGameMessage: Unit = print("\nChoose your game:\n 1: Human VS Human \n 2: Human VS Machine \n 3: Machine VS Machine\n")

    def showContinueMessage: Unit =  print("\nPress any key to continue\n")

    def showInvalidAnswer: Unit = print("\nInvalid answer. Try again.\n")

    def getUserInput(): String = readLine.trim.toUpperCase


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
      * ask to the user to choose his type of game.
      * 1 - Human VS Human
      * 2 - Human VS Machine
      * 3 - Machine VS Machine
      *
      * @return a new game
      */
    @tailrec
    def getChoiceGame: GameState = {
        showChoiceGameMessage
        val userInput = getUserInput()

        userInput match {
            case "1" =>  GameState(Player("Player1", fistPlayer = true, action = HumanAction), Player("Player2", action = HumanAction))
            case "2" => GameState(Player("Player1", fistPlayer = true, action = HumanAction), Player("Player2", action = AIAction))
            case "3" => GameState(Player("Player1", fistPlayer = true, action = AIAction), Player("Player2", action = AIAction))
            case _ => showInvalidAnswer; getChoiceGame
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
        val cell = activePlayer.shoot()

        val res = passivePlayer.addOpponentShoot(cell)
        val newPassivePlayer = res._1
        val shootRes = res._2

        shootRes match {
            case ShootResult.FLEETSUNK => {
                // increamente the score of the winner
                val activePlayerWinner = activePlayer.incrementScore()
                (activePlayerWinner, newPassivePlayer)
            }
            case _ => {
                val newActivePlayer = activePlayer.addOwnShoot(cell, shootRes)
                shootsLoop(newPassivePlayer, newActivePlayer)
            }
        }

//        // Take the state of the cell shoot
//        val cellState = passivePlayer.getCellStateShipsGrid(cell).get
//        println("cell state: " + cellState + "\n")

//        cellState match {
//
//            case CellState.EMPTY    => {
//                val newActivePlayer = activePlayer.updateShootsGrid(cell, CellState.MISS)
//                val newPassivePlayer = passivePlayer.addOpponentShoot(cell)
//                clear
//                println(newActivePlayer.shipsGrid + "\n")
//                println(newActivePlayer.shootsGrid)
//                showMissShotMessage
//                showContinueMessage
//                readLine()
//                shootsLoop(newPassivePlayer, newActivePlayer)
//            }
//
//
//            // If there is a ship, we update the fleet and the shipsGrid of the passive player
//            case CellState.SHIP     => {
//                val shipTouch = passivePlayer.shipTouched(cell).get
//                val newActivePlayer = activePlayer.updateShootsGrid(cell, CellState.TOUCH)
//                val newPassivePlayer = passivePlayer.addOpponentShoot(cell)
//                clear
//                println(newActivePlayer.shipsGrid + "\n")
//                println(newActivePlayer.shootsGrid)
//                showTouchShipMessage
//                // check if the ship sie is 1 and it was touched, we will be sunk.
//                if (shipTouch.willBeSunk) {
//                    showSunkShipMessage
//                    // check if the fleet is sunk ==> active player win
//                    if (newPassivePlayer.isFleetSunk()) {
//                        showSunkFleetMessage
//                        // increamente the score of the winner
//                        val activePlayerWinner = newActivePlayer.incrementScore()
//                        println("The game is finish")
//                        return (activePlayerWinner, newPassivePlayer)
//                    }
//                    else {
//                        showContinueMessage
//                        readLine()
//                        shootsLoop(newPassivePlayer, newActivePlayer)
//                    }
//                }
//                else {
//                    showContinueMessage
//                    readLine()
//                    shootsLoop(newPassivePlayer, newActivePlayer)
//                }
//            }
//            // Else, cell already shot
//            case _                  => {
//                clear
//                println(activePlayer.shipsGrid + "\n")
//                println(activePlayer.shootsGrid)
//                showAlreadyShotMessage
//                showContinueMessage
//                readLine()
//                shootsLoop(passivePlayer, activePlayer)
//            }
//        }
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