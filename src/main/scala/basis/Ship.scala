package basis

import game.Utility.Direction


/**
  * Manage data about a ship
  *
  * @param name the name of the ship.
  * @param position a set of cell corresponding to its position.
  */
case class Ship(name: String, position: Set[Cell] = Set()) {

    /**
      * Check if the cell belongs to the ship.
      *
      * @param cell. The cell to test.
      * @return Boolean. True if the cell belongs to the ship.
      */
    def isTouched(cell: Cell): Boolean = {
        position.contains(cell)
    }

    /**
      * Check if the ship is sunk.
      *
      * @return Boolean. True if the ships is sunk.
      */
    def isSunk: Boolean = position.isEmpty


    /**
      * Create a new ship without the cell given.
      * If the cell doesn't belong to the ship, simply return the ship.
      *
      * @param cell
      * @return a new ship that contains all the elements of this ship but that not contains cell.
      */
    def removeCell(cell: Cell): Ship = {

        if (isTouched(cell)) {
            val newPosition = position - cell
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
    def apply(firstCell: Cell, direction: String, descrShip: Tuple2[String,Int]): Option[Ship] = {

        if ( direction != Direction.HORIZONTAL && direction != Direction.VERTICAL) None
        else {
            val position: List[Cell] = {
                val size = descrShip._2
                direction match {
                    case Direction.HORIZONTAL => List.iterate(firstCell, size)(cell => Cell(cell.x, cell.y+1))
                    case Direction.VERTICAL => List.iterate(firstCell, size)(cell => Cell(cell.x+1, cell.y))
                }
            }
            Some(Ship(descrShip._1, position.toSet))
        }
    }
}

