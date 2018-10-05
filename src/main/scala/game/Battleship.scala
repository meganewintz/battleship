package game

import actors._
import game.Utility._


import scala.annotation.tailrec

case class GameState(p1: Player, p2: Player)

object Battleship extends App {
    // val ages = Seq(42, 75, 29, 64)
    //println(s"The oldest person is ${ages.max}")
    //var table = Array.fill(10,10)("-")
    // var table = Array.fill(10,10)(CellState.EMPTY)
    // table(2)(3) = CellState.TOUCH
    // table(3)(5) = CellState.MISS
    // table(1)(6) = CellState.SHIP
    // table(7)(7) = CellState.MISS
    //table(2)(2) = "x "
    //table(2)(3) = "o"
    //table(1)(2) = "5"
    //table(1)(3) = "-"
    //table foreach { row => row foreach print; print("  x"); println }
    //var c = List[Point]
    //  var p1 = new Cell(1,2)
    //  var p2 = new Cell(1,2)
    //  var fleet: List[Ship] = Nil
    //  var ship = new Ship(Direction.HORIZONTAL, p1, 4)
    // val player = new Player(new Grid(10), new Grid(10))
    // player.addShip(ship)
    // print(player.shipsGrid.toString)
    //print("\nchar:" + 1.toChar + "\n")

    //print("Res: " ,ship.coord.forall(p => p.x >= 0 && p.x < 10 && p.y >= 0 && p.y < 10 && table(p.x)(p.y).equals("-")))

    val game = GameState(Player("Player1", fistPlayer = true), Player("Player2"))

    mainLoop(game)

    @tailrec
    def mainLoop(game: GameState): Unit = {

        // create the fleet for each player
        showPlayerTour(game.p1)
        val player1 = createFleetPlayer(game.p1, descrShips)
        clear
        showPlayerTour(game.p2)

        val player2 = createFleetPlayer(game.p2, descrShips)

        // Loop for the shoot
        val players = shootsLoop(player1, player2)

        val startAgain = getPlayAgainActionUser

        if (startAgain) {
            //Reboot the players
            val newPlayer1 = player1.ressetPlayer()
            val newPlayer2 = player2.ressetPlayer()

            // Start again the battleship and changing the fist player.
            if (newPlayer1.fistPlayer) mainLoop(GameState(newPlayer1, newPlayer2))
            else mainLoop(GameState(newPlayer2, newPlayer1))
        }

    }





//    val emily1 = Person("Emily", "Maness")
//    val emily2 = emily1.copy(lastname = "Wells")
//    //print(emily1.lastname)
//    //print(emily2.lastname)
//    val set = Set(Tuple2(1, 2), Tuple2(2,3))
//    val newShip = set - Tuple2(2,3)
//    //print("\nLa taille: " + ship.size)
//    //print("\nLa taille: " + newShip.size)
//    val griid = Grid(List.fill(10,10)(CellState.EMPTY))
//    val coord = griid.cells
//
//    val listCell = griid.cells.apply(2)
//    val newListCell = listCell.updated(2, CellState.TOUCH)
//    val newCells = griid.cells.updated(1, newListCell)
//    val newGrid = griid.copy(cells = newCells)
//    val nGrid = GridUtil.updateCellState(griid, (1,1), CellState.SHIP)
//    val nnGrid = GridUtil.updateMultipleCellsState(nGrid, Set((1,3), (1,4),(1,5)), CellState.SHIP)
//    //print(nGrid + "\n\n")
//    print(nnGrid)
//
//    val player = Player("Mégane")
//    val ship = Ship("Gros bateau", Set((1, 2), (1,3), (1,4)))
//    val newPlayer = PlayerUtil.addShipToPlayer(player, ship)
//    //print(newPlayer.shipsGrid)
//    //print("\nFleet: " + newPlayer.fleet.length)




}
/*
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
    state = SHIP -> value = ??]
 */