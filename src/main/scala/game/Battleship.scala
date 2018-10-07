package game

import action.HumanAction
import actors._
import game.Utility._

import scala.annotation.tailrec

case class GameState(p1: Player, p2: Player, loop: Int = 0)

object Battleship extends App {

    val game = getChoiceGame

    mainLoop(game)

    @tailrec
    def mainLoop(game: GameState): Unit = {

        // create the fleet for each player
        val player1 = game.p1.initialiseFleet(descrShips)
        val player2 = game.p2.initialiseFleet(descrShips)

        // Loop for the shoot
        val players = shootsLoop(player1, player2)
        showPlayerWinnerMessage(players._1)
        showScorePlayer(players._1)
        showScorePlayer(players._2)

        //Reboot the players
        val newPlayer1 = players._1.ressetPlayer()
        val newPlayer2 = players._2.ressetPlayer()


        if (game.loop > 1) {

            // Start again the battleship and changing the fist player.
            if (newPlayer1.fistPlayer) mainLoop(GameState(newPlayer1, newPlayer2, game.loop-1))
            else mainLoop(GameState(newPlayer2, newPlayer1, game.loop-1))
        }
        // An human play: we ask to him if he wants to play again.
        else if (player1.action == HumanAction || player2.action == HumanAction) {
            val startAgain = getPlayAgainActionUser

            if (startAgain) {

                // Start again the battleship and changing the fist player.
                if (newPlayer1.fistPlayer) mainLoop(GameState(newPlayer1, newPlayer2))
                else mainLoop(GameState(newPlayer2, newPlayer1))
            }
        }
    }

}
/*

Human
play


AI vs AI
mainLoop x100 donc on sort quand loop = 0
Donc: on ne demande pas au joueur start again, on enchaine direct
 */