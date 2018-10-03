package helpers

import players._
import game.GameSettings
import boats._

object DisplayHelper {
    def rules(): Unit = {
        println("Ici on écris les règles")
    } 

    def boatOutGrid(): Unit = {
        println("The boat is out of the grid, retry.")
    }

    def errorCrossingBoat(): Unit = {
        println("The boat is crossing another, retry.")
    }

    def playerTurn(name: String): Unit = {
        println(name + " turn :")
    }

    def shotOutGrid(): Unit = {
        println("The shot is out of the grid. Retry.")
    }

    def shotSuccess(): Unit = {
        println("The shot succeed !")
    }

    def shotFailure(): Unit = {
        println("The shot failed !")
    }

    def sunkBoat(boat: Boat): Unit = {
        println("Boat of size " + boat.size() + " has been sank !")
    }

    def playerWin(playerName: String): Unit = {
        println(playerName + " win the game !")
    }

    def alreadyShot(): Unit = {
        println("You already shot this position. Retry.")
    }

    def grids(player: Player): Unit = {
        val gridSize: Int = GameSettings.gridSize
        val boatCells: List[Cell] = player.boats.flatMap((b) => b.cells)

        def displayGrid(x: Int, y: Int, gridSize: Int, boatCells: List[Cell], shots: List[Shot]): Unit = {
            //Numbers on top
            if(x <= gridSize && y == gridSize+1){
                print(" " + x)
                displayGrid(x+1, y, gridSize, boatCells, shots)
            }
            //Last top Cell
            else if(x == gridSize+1 && y == gridSize+1){
                println()
                displayGrid(1, y-1, gridSize, boatCells, shots)
            }
            //Normal Cell
            else if(x <= gridSize && y > 0){
                val filteredCells: List[Cell] =  boatCells.filter((s) => s.x == x && s.y == y)
                val filteredShots: List[Shot] =  shots.filter((rs) => rs.x == x && rs.y == y)
                if(filteredCells.length > 0){
                    val cell: Cell = filteredCells.head
                    print("|" + cell.state)
                }
                else if(filteredShots.length > 0){
                    if(filteredShots.head.touched) 
                        print("|" + GameSettings.touchedDisplay)
                    else 
                        print("|" + GameSettings.untouchedDisplay)
                }
                else{
                    print("| ")
                }
                displayGrid(x+1, y, gridSize, boatCells, shots)
            }
            //Numbers on right
            else if(x >= gridSize && y > 0 && y < gridSize+1){
                println("| " + y)
                displayGrid(1, y-1, gridSize, boatCells, shots)
            }
        }

        println(player.name + " boat grid :")
        displayGrid(1, gridSize+1, gridSize, boatCells, player.receivedShots)
        println(player.name + " shots grid :")
        displayGrid(1, gridSize+1, gridSize, List(), player.sentShots)
    }
}