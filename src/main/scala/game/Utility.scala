package game


import actions._
import basis._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.reflect.io.File
import scala.sys.process._

object CellState extends Enumeration {
    val EMPTY = Value("-")
    val MISS = Value(Console.RED + "X" + Console.RESET)
    val TOUCH = Value(Console.BLUE + "O" + Console.RESET)
    val SHIP = Value("s")
}

/**
  * The shoot results available.
  */
object ShotResult extends Enumeration {
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
      * The ships list for the game: name and size.
      */
    val battleAIs = List(
        GameState(Player("AI Level Medium", fistPlayer = true, action = AI2), Player("AI Level Hard", action = AI3), loop = 100),
        GameState(Player("AI Level Beginner", fistPlayer = true, action = AI1), Player("AI Level Hard", action = AI3), loop = 100),
        GameState(Player("AI Level Beginner", fistPlayer = true, action = AI1), Player("AI Level Medium", action = AI2), loop = 100)


    )


    /**
      * To clear the termiNal
      */
    def clear: Unit = println("clear".!)

    def showPlayerTour(player: Player): Unit = println("\n" + player.name + " it's your turn.")

    def showPlayerWinnerMessage(player: Player): Unit = println("\n" + player.name + " win the game!")

    def showPlaceShipMessage(player: Player, name: String, sizeShip: Int): Unit = println("\n" + player.name + " place the " + name + " of size " + sizeShip + "")

    def showPromptStartCellShip(): Unit = println("\nEnter the cell of the ship. Ex: A1 :")

    def showPromptDirectionShip(): Unit = println("\nEnter the direction of the ship (H)orizontal or (V)ertical :")

    def showPromptCell(): Unit = println("\nEnter the target cell. Ex: A1 :")

    def showInvaliDirectionMessage: Unit = println("\nThe direction enter is invalid.")

    def showInvalidPlacementMessage: Unit = println("\n /!\\ Your ship is out of bouds or there is already a ship placed. Try again. ")

    def showInvalidCoordMessage: Unit = println("\nThe coordinates are invalid. Try again.")

    def showPlayAgainMessage: Unit = println("\nDo you want to play again? (Y)es/(N)o.")

    def showChoiceGameMessage: Unit = println("\nChoose your game:\n 1: Human VS Human \n 2: Human VS Machine")

    def showChoiceAILevelMessage: Unit = println("\nChoose the level of the machine:\n 1: level Beginner \n 2: level Medium \n 3: level Hard")

    def showContinueMessage: Unit = println("\nPress any key to continue.")

    def showInvalidAnswer: Unit = println("\nInvalid answer. Try again.");

    def showScorePlayer(player: Player): Unit = println("\n" + player.name + ": " + player.score + " points")

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
            case PlayAgain.YES => true
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
            case "1" => GameState(Player("Player1", fistPlayer = true, action = HumanAction), Player("Player2", action = HumanAction))
            case "2" => levelChoice
            case _ => showInvalidAnswer; getChoiceGame
        }
    }

    def levelChoice: GameState = {
        showChoiceAILevelMessage
        val userInput = getUserInput()

        userInput match {
            case "1" => GameState(Player("Player", fistPlayer = true, action = HumanAction), Player("AI Level Beginner", action = AI1))
            case "2" => GameState(Player("Player", fistPlayer = true, action = HumanAction), Player("AI Level Medium", action = AI2))
            case "3" => GameState(Player("Player", fistPlayer = true, action = HumanAction), Player("AI Level Hard", action = AI3))
            case _ => showInvalidAnswer; getChoiceGame
        }
    }

    /**
      * Loop of the shoots player. The loop finish when a player win, the adverse fleet is sunk.
      *
      * @param activePlayer  the active player
      * @param passivePlayer the passive player
      * @return (Player,Player) the first player represent the looser and the second the winner
      */
    def shootsLoop(activePlayer: Player, passivePlayer: Player): Tuple2[Player, Player] = {

        // Ask the shoot cell to the active player
        val cell = activePlayer.shoot()

        val res = passivePlayer.addOpponentShoot(cell)
        val newPassivePlayer = res._1
        val shootRes = res._2

        shootRes match {
            case ShotResult.FLEETSUNK => {
                // increamente the score of the winner
                val activePlayerWinner = activePlayer.incrementScore()
                (newPassivePlayer, activePlayerWinner)
            }
            case _ => {
                val newActivePlayer = activePlayer.addOwnShoot(cell, shootRes)
                shootsLoop(newPassivePlayer, newActivePlayer)
            }
        }
    }

    /**
      * Print in a csv file named "ai_proof.csv" the result of all the gone battles.
      * @param goneBattle
      */
    def csvPrint(goneBattle: List[GameState]): Unit = {
        File("ai_proof.csv").writeAll("AI name;AI score;AI name2;AI score2\n")
        val csv: String = goneBattle.map( g => g.p1.name + ";" + g.p1.score + ";" + g.p2.name + ";" + g.p2.score + "\n").reduce(_+_)
        File("ai_proof.csv").appendAll(csv)
    }


}