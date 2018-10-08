import org.scalatest._
import actors._

class ShipTest extends FlatSpec with Matchers {

    val ship = Ship("Imperator", Set(Cell(1,1), Cell(1,2), Cell(1,3), Cell(1,4)))

    // isTouched
    // -----------------------------------------------------------------------

    it should "return true because the ship is touched" in {
        ship.isTouched(Cell(1,1)) shouldEqual true
    }

    it should "return false because the ship isn't touched" in {
        ship.isTouched(Cell(1,6)) shouldEqual false
    }

    // isSunk
    // -----------------------------------------------------------------------

    val shipSunk = Ship("Smaller", Set())

    it should "return true because the ship is sunk (empty)" in {
        shipSunk.isSunk shouldEqual true
    }

    it should "return false because the ship isn't sunk" in {
        ship.isSunk shouldEqual false
    }

    // removeSquareShip
    // -----------------------------------------------------------------------

    val shipMinus = Ship("Imperator", Set(Cell(1,2), Cell(1,3), Cell(1,4)))

    it should "remove a cell to the ship" in {
        ship.removeSquareShip(Cell(1,1)) shouldEqual shipMinus
    }

    // willBeSunk
    // -----------------------------------------------------------------------

    it should "return false because the ship will be sunk" in {
        ship.willBeSunk shouldEqual false
    }

    it should "return true because the ship will be sunk" in {
        val shipDead = Ship("Imperator", Set(Cell(1,1)))
        shipDead.willBeSunk shouldEqual true
    }


}