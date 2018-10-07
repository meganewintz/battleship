import action.HumanAction
import org.scalatest._
import actors._
import game.CellState

class PlayerTest extends FlatSpec with Matchers {

    val playerEmpty = Player("Megmeg", action = HumanAction)
    val ship = Ship("Imperator", Set((1,1), (1,2), (1,3), (1,4)))
    val shipSize1 = Ship("PetitUn", Set((4,4)))
    val player = playerEmpty.addShipToPlayer(ship)
    val shipOut = Ship("Imperator", Set((1,11), (1,12), (1,13), (1,14)))

    val fleet = List(ship)
    val emptyGrid = Grid(List.fill(10,10)(CellState.EMPTY))

    // getCellStateShipsGrid
    // -----------------------------------------------------------------------

    it should "get the state of cell in the shipsGrid" in {
        player.getCellStateShipsGrid(1,1) shouldEqual Some(CellState.SHIP)
    }

    it should "return None because the cell isn't in the shipsGrid" in {
        player.getCellStateShipsGrid(1,12) shouldEqual None
    }

    // getCellStateShootsGrid
    // -----------------------------------------------------------------------

    it should "get the state of cell in the shootsGrid" in {
        player.getCellStateShootsGrid(1,1) shouldEqual Some(CellState.EMPTY)
    }

    it should "return None because the cell isn't in the shootsGrid" in {
        player.getCellStateShootsGrid(1,12) shouldEqual None
    }



    // isShipPlaceable
    // -----------------------------------------------------------------------

    it should "place a ship if its position is correct" in {
        playerEmpty.isShipPlaceable(ship) shouldEqual true
    }

    it should "not place a ship if its position is out of bound" in {
        playerEmpty.isShipPlaceable(shipOut) shouldEqual false
    }

    it should "not place a ship if one of its square is on another ship " in {

        val ship2 = Ship("Imperator", Set((1,1), (2,1), (3,1), (4,1)))
        player.isShipPlaceable(ship2) shouldEqual false
    }

    // shipTouched
    // -----------------------------------------------------------------------

    it should "say that the specific ship is touch" in {
        player.shipTouched(1,3) shouldEqual Some(ship)
    }

    it should "return None because no ship was touched" in {
        player.shipTouched(7,7) shouldEqual None
    }

    // isShipSunk
    // -----------------------------------------------------------------------

    it should "say that the specific ship is sunk" in {
        val shipSunk = Ship("Imperator", Set())
        val playerShipSunk = playerEmpty.addShipToPlayer(shipSunk)

        playerShipSunk.isShipSunk(shipSunk) shouldEqual true
    }

    it should "say that the specific ship is NOT sunk" in {
        player.isShipSunk(ship) shouldEqual false
    }

    // isFleetSunk
    // -----------------------------------------------------------------------

    it should "return true because the fleet is sunk" in {
        playerEmpty.isFleetSunk shouldEqual true
    }

    it should "return false because the fleet is NOT sunk" in {
        player.isFleetSunk shouldEqual false
    }

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
        val shipsGrid = emptyGrid.updateMultipleCellsState(Set((1,1), (1,2), (1,3), (1,4)), CellState.SHIP)
        playerEmpty.addShipToPlayer(ship) shouldEqual Player("Megmeg", action = HumanAction, shipsGrid = shipsGrid, fleet = fleet)
    }

    it should "return the initial player because the ship touched another ship of the payer" in {
        val ship2 = Ship("Cruiser", Set((1,4), (1,5)))
        player.addShipToPlayer(ship2) shouldEqual player
    }

    it should "return the initial player because the ship is out of the grid" in {
        player.addShipToPlayer(shipOut) shouldEqual player
    }

    // removeShipToPlayer
    // -----------------------------------------------------------------------

    it should "create a new Player without a ship in its fleet" in {
        val shipsGrid = emptyGrid.updateMultipleCellsState(Set((1,1), (1,2), (1,3), (1,4)), CellState.SHIP)
        player.removeShipToPlayer(ship) shouldEqual Player("Megmeg", action = HumanAction, shipsGrid = shipsGrid, fleet = List())
    }

    // updateShipsGrid
    // -----------------------------------------------------------------------

    it should "create a new player with its shipsGrid updated" in {
        val playerUp = player.updateShipsGrid((1,1), CellState.TOUCH)
        playerUp.getCellStateShipsGrid(1,1) shouldEqual Some(CellState.TOUCH)
    }

    it should "return the initial player because the cell does NOT belong to the shipsGrid" in {
        val playerUp = player.updateShipsGrid((1,12), CellState.TOUCH)
        playerUp shouldEqual player
    }

    // updateShootsGrid
    // -----------------------------------------------------------------------

    it should "create a new player with its shootsGrid updated" in {
        val playerUp = player.updateShootsGrid((1,1), CellState.TOUCH)
        playerUp.getCellStateShootsGrid(1,1) shouldEqual Some(CellState.TOUCH)
    }

    it should "return the initial player because the cell does NOT belong to the shootsGrid" in {
        val playerUp = player.updateShootsGrid((1,12), CellState.TOUCH)
        playerUp shouldEqual player
    }

    // fleetTouched
    // -----------------------------------------------------------------------

    it should "create a new player by removing the square of the ship touched" in {
        val playerUp = player.fleetTouched((1,1))
        (
            player.shipTouched(1,1) shouldEqual Some(ship),
            playerUp.shipTouched(1,1) shouldEqual None
        )
    }

    it should "create a new player by removing the ship touched because it is sunk" in {
        val player2Ships = player.addShipToPlayer(shipSize1)
        val playerUp = player2Ships.fleetTouched((4,4))
        (
            playerUp.shipTouched(4,4) shouldEqual None,
            playerUp.fleet.length shouldEqual 1
        )
    }

    it should "create a new player with its fleet sunk because we just sunk the last ship" in {
        val playerSmall = playerEmpty.addShipToPlayer(shipSize1)
        val playerUp = playerSmall.fleetTouched((4,4))
        (
            playerUp.shipTouched(4,4) shouldEqual None,
            playerUp.isFleetSunk() shouldEqual true
        )
    }

    it should "return the initial player because no ships was touched" in {
        val playerUp = player.fleetTouched((4,4))
        playerUp shouldEqual player
    }

    // addAdverseShoot
    // -----------------------------------------------------------------------

//    it should "update the cell touched by the opponent (MISS)" in {
//        val playerTouchedMiss = player.addOpponentShoot((2,2))
//        playerTouchedMiss.getCellStateShipsGrid(2,2) shouldEqual Some(CellState.MISS)
//    }
//
//    it should "update the cell touched by the opponent (TOUCH)" in {
//        val playerTouchedTouch = player.addOpponentShoot((1,2))
//        (
//            playerTouchedTouch.getCellStateShipsGrid(1,2) shouldEqual Some(CellState.TOUCH),
//            playerTouchedTouch.shipTouched(1,2) shouldEqual None
//        )
//    }
//
//    it should "return the initial player because the cell was already touched" in {
//        val playerTouchedTouch = player.addOpponentShoot((1,2))
//        val playerAlreadyTouch = player.addOpponentShoot((1,2))
//        playerTouchedTouch shouldEqual playerAlreadyTouch
//    }
//
//    it should "return the initial player because the cell is NOT belong to the shipsGrid" in {
//        val playerTouchedOut = player.addOpponentShoot((1,13))
//        playerTouchedOut shouldEqual player
//    }

}
