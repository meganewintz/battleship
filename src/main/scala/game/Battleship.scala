package game

import actors._
import game.Utility._


import scala.annotation.tailrec

case class GameState(p1: Player, p2: Player)

object Battleship extends App {

    val game = getChoiceGame

    mainLoop(game)

    @tailrec
    def mainLoop(game: GameState): Unit = {

        // create the fleet for each player
        showPlayerTour(game.p1)
        val player1 = game.p1.initialiseFleet(descrShips)

        showPlayerTour(game.p2)
        val player2 = game.p2.initialiseFleet(descrShips)

        // Loop for the shoot
        val players = shootsLoop(player1, player2)

        showPlayerWinnerMessage(player1)

        val startAgain = getPlayAgainActionUser

        if (startAgain) {
            //Reboot the players
            val newPlayer1 = player1.ressetPlayer()
            val newPlayer2 = player2.ressetPlayer()

            // Start again the battleship and changing the fist player.
            if (newPlayer1.fistPlayer) mainLoop(GameState(newPlayer1, newPlayer2))
            else mainLoop(GameState(newPlayer2, newPlayer1))
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


Idea cell:
A cell has :
    - x
    - y
    - value
    - state

def a state and a value on each cell.
- state = EMPTY -> transform it to MISS
- state = MISS or HIT -> enter coord again
- state = SHIP -> check in the fleet which ship was touched

state = EMPTY -> value = "-"
    state = MISS -> value = "x"
    state = HIT -> value = "o"
    state = EMPTY -> value = "-"
    state = SHIP -> value = ??]
 */