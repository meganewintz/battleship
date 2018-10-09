package game

import actions.{AI3, HumanAction}
import basis._
import game.Utility._

import scala.reflect.io.File

import scala.annotation.tailrec


case class GameState(p1: Player, p2: Player, loop: Int = 0)

object Battleship extends App {

    // ti launch the test of the AIs.
    if (args.length > 0 && args(0) == "testAIs") {
        testAIloop(battleAIs, List())
    }
    else {
        val game = getChoiceGame
        mainLoop(game)
    }


    @tailrec
    def mainLoop(game: GameState): GameState = {

        // create the fleet for each player
        val player1 = game.p1.initialiseFleet(descrShips)
        val player2 = game.p2.initialiseFleet(descrShips)

        // Loop for the shoot
        val players = shootsLoop(player1, player2)
        showPlayerWinnerMessage(players._2)
        showScorePlayer(players._2)
        showScorePlayer(players._1)

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
            else GameState(newPlayer1, newPlayer2)
        }
        else GameState(newPlayer1, newPlayer2)
    }

    /**
      * The loop for the test between the AIs.
      * We loop until there is remainingBattle.
      * At the end of the loop, we print the result of the battles in a csv.
      *
      * @param remainingBattle
      * @param goneBattle
      */
    @tailrec
    def testAIloop(remainingBattle: List[GameState], goneBattle: List[GameState]): Unit = {

        if (remainingBattle.nonEmpty) {
            val resBattle = mainLoop(remainingBattle.head)
            testAIloop(remainingBattle.tail, resBattle :: goneBattle)
        }
        else {
            csvPrint(goneBattle)
        }
    }
}
