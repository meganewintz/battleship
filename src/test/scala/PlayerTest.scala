
import actions._
import org.scalatest._
import basis._
import game.CellState
import game.ShotResult

class PlayerTest extends FlatSpec with Matchers {

    val playerEmpty = Player("Megmeg", action = HumanAction)
    val ship = Ship("Imperator", Set(Cell(1,1), Cell(1,2), Cell(1,3), Cell(1,4)))
    val shipSize1 = Ship("PetitUn", Set(Cell(4,4)))
    val player = playerEmpty.addShipToPlayer(ship)
    val shipOut = Ship("Imperator", Set(Cell(1,11), Cell(1,12), Cell(1,13), Cell(1,14)))

    val fleet = List(ship)
    val emptyGrid = Grid(List.fill(10,10)(CellState.EMPTY))


    // getCellStateShotsGrid
    // -----------------------------------------------------------------------

    it should "get the state of cell in the shotsGrid" in {
        player.getCellStateShotsGrid(Cell(1,1)) shouldEqual Some(CellState.EMPTY)
    }

    it should "return None because the cell isn't in the shotsGrid" in {
        player.getCellStateShotsGrid(Cell(1,12)) shouldEqual None
    }



    // isShipPlaceable
    // -----------------------------------------------------------------------

    it should "place a ship if its position is correct" in {
        playerEmpty.isShipPlaceable(ship) shouldEqual true
    }

    it should "not place a ship if its position is out of bound" in {
        playerEmpty.isShipPlaceable(shipOut) shouldEqual false
    }

    it should "not place a ship if one of its cell is on another ship " in {

        val ship2 = Ship("Imperator", Set(Cell(1,1), Cell(2,1), Cell(3,1), Cell(4,1)))
        player.isShipPlaceable(ship2) shouldEqual false
    }

//    // shipTouched
//    // -----------------------------------------------------------------------
//
//    it should "say that the specific ship is touch" in {
//        player.shipTouched(Cell(1,3)) shouldEqual Some(ship)
//    }
//
//    it should "return None because no ship was touched" in {
//        player.shipTouched(Cell(7,7)) shouldEqual None
//    }
//
//    // isShipSunk
//    // -----------------------------------------------------------------------
//
//    it should "say that the specific ship is sunk" in {
//        val shipSunk = Ship("Imperator", Set())
//        val playerShipSunk = playerEmpty.addShipToPlayer(shipSunk)
//
//        playerShipSunk.isShipSunk(shipSunk) shouldEqual true
//    }
//
//    it should "say that the specific ship is NOT sunk" in {
//        player.isShipSunk(ship) shouldEqual false
//    }
//
//    // isFleetSunk
//    // -----------------------------------------------------------------------
//
//    it should "return true because the fleet is sunk" in {
//        playerEmpty.isFleetSunk shouldEqual true
//    }
//
//    it should "return false because the fleet is NOT sunk" in {
//        player.isFleetSunk shouldEqual false
//    }
//
    // incrementScore
    // -----------------------------------------------------------------------

    it should "incremente the score of the player" in {
        val playerScore1 = playerEmpty.incrementScore()
        playerScore1.score shouldEqual 1
    }

    // ressetPlayer
    // -----------------------------------------------------------------------

    it should "resset the player" in {
        val playerWithShipScore1 = player.incrementScore()
        val playerResset = playerWithShipScore1.ressetPlayer()
        (
            playerResset.name shouldEqual "Megmeg",
            playerResset.score shouldEqual 1,
            playerResset.fleet.isEmpty shouldEqual true,
        )
    }

    // addShipToPlayer
    // -----------------------------------------------------------------------

    it should "create a new Player with a new ship in its fleet and update his shipsGrid" in {
        val shipsGrid = emptyGrid.updateMultipleCellsState(Set(Cell(1,1), Cell(1,2), Cell(1,3), Cell(1,4)), CellState.SHIP)
        playerEmpty.addShipToPlayer(ship) shouldEqual Player("Megmeg", action = HumanAction, shipsGrid = shipsGrid, fleet = fleet)
    }

    it should "return the initial player because the ship touched another ship of the payer" in {
        val ship2 = Ship("Cruiser", Set(Cell(1,4), Cell(1,5)))
        player.addShipToPlayer(ship2) shouldEqual player
    }

    it should "return the initial player because the ship is out of the grid" in {
        player.addShipToPlayer(shipOut) shouldEqual player
    }

    // addAdverseShoot
    // -----------------------------------------------------------------------

    it should "return the result of the shot (MISS)" in {
        val playerTouched = player.addOpponentShoot(Cell(2,2))
        playerTouched._2 shouldEqual ShotResult.MISS
    }

    it should "return the result of the shot (TOUCH)" in {
        val playerTouched = player.addOpponentShoot(Cell(1,2))
        playerTouched._2 shouldEqual ShotResult.TOUCH
    }

    it should "return the result of the shot (SHIPSUNK)" in {
        val playerWith2Ships = player.addShipToPlayer(shipSize1)
        val playerTouched = playerWith2Ships.addOpponentShoot(Cell(4,4))
        playerTouched._2 shouldEqual ShotResult.SHIPSUNK
    }

    it should "return the result of the shot (FLEETSUNK)" in {
        val player1ship = playerEmpty.addShipToPlayer(shipSize1)
        val playerTouched = player1ship.addOpponentShoot(Cell(4,4))
        playerTouched._2 shouldEqual ShotResult.FLEETSUNK
    }

    it should "return the result of the shot (ALREADYSHOT)" in {
        val playerTouched = player.addOpponentShoot(Cell(1,2))._1
        val playerTouched2 = playerTouched.addOpponentShoot(Cell(1,2))
        playerTouched2._2 shouldEqual ShotResult.ALREADYSHOOT
    }

    it should "return (the initial player, ALREADYTOUCHED) because the cell was already touched" in {
        val playerTouchedTouch = player.addOpponentShoot(Cell(1,2))
        val playerAlreadyTouch = playerTouchedTouch._1.addOpponentShoot(Cell(1,2))
        playerAlreadyTouch shouldEqual (playerTouchedTouch._1, ShotResult.ALREADYSHOOT)
    }

    it should "return the initial player because the cell is NOT belong to the shipsGrid" in {
        val playerTouchedOut = player.addOpponentShoot(Cell(1,13))
        playerTouchedOut shouldEqual (player, ShotResult.MISS)
    }

}

