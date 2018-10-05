package actors

import game.Utility.Direction

case class Ship(name: String, position: Set[Tuple2[Int,Int]] = Set()) {

    /**
      * Check if the square belongs to the ship.
      *
      * @param square. The square to test.
      * @return Boolean. True if the square belongs to the ship.
      */
    def isTouched(square: Tuple2[Int,Int]): Boolean = {
        position.contains(square)
    }

    /**
      * Check if the ship is sunk.
      *
      * @return Boolean. True if the ships is sunk.
      */
    def isSunk: Boolean = position.isEmpty

    /**
      * Check if the ship will be sunk if it is touch one more time.
      *
      * @return true if the ship will be sunk if it is touch one more time.
      */
    def willBeSunk: Boolean = position.size == 1

    /**
      * Create a new ship without the square given.
      * If the square doesn't belong to the ship, simply return the ship.
      *
      * @param square
      * @return a new ship that contains all the elements of this ship but that not contains square.
      */
    def removeSquareShip(square: Tuple2[Int,Int]): Ship = {

        if (isTouched(square)) {
            val newPosition = position - square
            copy(position = newPosition)
        }
        else this
    }

}

object Ship {
    /**
      * Create a ship according to its initial cell, its direction and its description
      *
      * @param firstCell
      * @param direction
      * @param descrShip
      * @return a ship
      */
    def apply(firstCell: Tuple2[Int,Int], direction: String, descrShip: Tuple2[String,Int]): Option[Ship] = {

        if ( direction != Direction.HORIZONTAL && direction != Direction.VERTICAL) None
        else {
            val position: List[Tuple2[Int, Int]] = {
                val size = descrShip._2
                direction match {
                    case Direction.HORIZONTAL => List.iterate(firstCell, size)(cell => (cell._1, cell._2 + 1))
                    case Direction.VERTICAL => List.iterate(firstCell, size)(cell => (cell._1 + 1, cell._2))
                }
            }
            Some(Ship(descrShip._1, position.toSet))
        }
    }
}



//object Ship {
//
//}
/*

Ship
- isTouched(coord)          OK
- isSunk                    OK
- removeCoord(coord)        OK

Grid
- getCellState(coord)       OK // gérer le cas ou les coord sont ext à la grille
- updateCellState(coord)    OK // gérer le cas ou les coord sont ext à la grille

Player
- addShip(ship)             OK
- removeShip(ship)          OK
- updateShip(ship)          OK
- updateGrid                OK






In a functional language, a functions are first-class citizens.

This means: a function has a value even if it is not applied.

- Faire le lien coord A1 vers Point(0,0)
- isTouched
- isSunk
- update shipGrid j2
- update shootGrid j1

GRID:
- display grid
- updateCell

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
    state = SHIP -> value = ??

If there is a ship, check in the fleet which ship was touched
 */

