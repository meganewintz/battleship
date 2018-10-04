import org.scalatest._
import actors._
import game.CellState

class PlayerTest extends FlatSpec with Matchers {

    val player = Player("Megmeg")
    val ship = Ship("Imperator", Set((1,1), (1,2), (1,3), (1,4)))
    val shipOut = Ship("Imperator", Set((1,11), (1,12), (1,13), (1,14)))


    val fleet = List(ship)
    val emptyGrid = Grid(List.fill(10,10)(CellState.EMPTY))


    it should "add a ship to the player fleet and update his shipsGrid" in {
        val shipsGrid = GridUtil.updateMultipleCellsState(emptyGrid, Set((1,1), (1,2), (1,3), (1,4)), CellState.SHIP)
        PlayerUtil.addShipToPlayer(player, ship) shouldEqual Player("Megmeg", shipsGrid = shipsGrid, fleet = fleet)
    }

    val playerWithShip = PlayerUtil.addShipToPlayer(player, ship)
    it should "remove a ship to the player fleet" in {
        val shipsGrid = GridUtil.updateMultipleCellsState(emptyGrid, Set((1,1), (1,2), (1,3), (1,4)), CellState.SHIP)
        PlayerUtil.removeShipToPlayer(playerWithShip, ship) shouldEqual Player("Megmeg", shipsGrid = shipsGrid, fleet = List())
    }

    it should "place a ship if its position is correct" in {
        player.isShipPlaceable(ship) shouldEqual true
    }

    it should "not place a ship if its position is out of bound" in {
        player.isShipPlaceable(shipOut) shouldEqual false
    }

    it should "not place a ship if one of its square is on another ship " in {
        val playerWtihShip = PlayerUtil.addShipToPlayer(player, ship)
        val ship2 = Ship("Imperator", Set((1,1), (2,1), (3,1), (4,1)))
        playerWtihShip.isShipPlaceable(ship2) shouldEqual false
    }

    it should "say that the specific ship is sunk" in {
        val shipSunk = Ship("Imperator", Set())
        val playerWtihShip = PlayerUtil.addShipToPlayer(player, shipSunk)

        playerWtihShip.isShipSunk(shipSunk) shouldEqual true
    }

    it should "say that the specific ship is NOT sunk" in {
        val playerWtihShip = PlayerUtil.addShipToPlayer(player, ship)

        playerWtihShip.isShipSunk(ship) shouldEqual false
    }

    it should "say that the specific ship is touch" in {
        val playerWtihShip = PlayerUtil.addShipToPlayer(player, ship)

        playerWtihShip.shipTouched(2,3) shouldEqual false
    }
}
