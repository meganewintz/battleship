import org.scalatest._
import actors._

//class ShipTest extends FunSuite{
//  test("Ship.constructor") {
//    assert()
//  }
//}
//
//
//class HelloSpec extends FunSuite with DiagrammedAssertions {
//  test("Hello should start with H") {
//    assert("hello".startsWith("H"))
//  }
//
//
//}

class ShipTest extends FlatSpec with Matchers {

    val ship = Ship("Imperator", Set((1,1), (1,2), (1,3), (1,4)))

    it should "return true because the ship is touched" in {
        ship.isTouched((1,1)) shouldEqual true
    }

    it should "return false because the ship isn't touched" in {
        ship.isTouched((1,6)) shouldEqual false
    }

    // -----------------------------------------------------------------------

    val shipSunk = Ship("Smaller", Set())

    it should "return true because the ship is sunk (empty)" in {
        shipSunk.isSunk shouldEqual true
    }

    it should "return false because the ship isn't sunk" in {
        ship.isSunk shouldEqual false
    }

    // -----------------------------------------------------------------------

    val shipMinus = Ship("Imperator", Set((1,2), (1,3), (1,4)))

    it should "remove a square to the ship" in {
        ShipUtil.removeSquareShip(ship, (1,1)) shouldEqual shipMinus
    }


}