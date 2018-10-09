import org.scalatest.{FlatSpec, Matchers}

import actions._
import basis._
import game.Utility._
import game._

class AI3Test extends FlatSpec with Matchers {

    val player = Player("Megmeg", action = AI3)
    val playerTouch = player.addOwnShoot(Cell(5,5), ShotResult.TOUCH)


    // shoot
    // -----------------------------------------------------------------------

    it should "shoot the cell on the right of the prev shot" in {
        playerTouch.shoot() shouldEqual Cell(5,6)
    }

    it should "shoot the cell on the left because the cell on the right is miss" in {
        val playerTouch1 = playerTouch.addOwnShoot(Cell(5,6), ShotResult.MISS)
        playerTouch1.shoot() shouldEqual Cell(5,4)
    }

    it should "shoot the cell on the bottom because the cell on the right, left and up are miss. After the previous shots: MISS MISS MISS TOUCH" in {
        val playerTouchRight = playerTouch.addOwnShoot(Cell(5,6), ShotResult.MISS)
        val playerTouchLeft = playerTouchRight.addOwnShoot(Cell(5,4), ShotResult.MISS)
        val playerTouchUp = playerTouchLeft.addOwnShoot(Cell(4,5), ShotResult.MISS)
        playerTouchUp.shoot() shouldEqual Cell(6,5)
    }

    it should "shoot the next cell available, aligned to the two previous shot cells " in {
        val playerTouchUp = playerTouch.addOwnShoot(Cell(4,5), ShotResult.TOUCH)
        playerTouchUp.shoot() shouldEqual Cell(3,5)
    }

    it should "shoot the next cell available, aligned to the two previous shot cells. After the previous shots: TOUCH MISS MISS MISS TOUCH " in {
        val playerTouchRight = playerTouch.addOwnShoot(Cell(5,6), ShotResult.MISS)
        val playerTouchLeft = playerTouchRight.addOwnShoot(Cell(5,4), ShotResult.MISS)
        val playerTouchUp = playerTouchLeft.addOwnShoot(Cell(4,5), ShotResult.MISS)
        val playerTouchDown = playerTouchUp.addOwnShoot(Cell(6,5), ShotResult.TOUCH)

        playerTouchDown.shoot() shouldEqual Cell(7,5)
    }

}
