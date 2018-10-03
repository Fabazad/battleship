package helpers

import players.Player
import game.GameSettings
import boats.Square

object DisplayHelper {
    def rules(gs: GameSettings): Unit = {
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
        println("The shot is out of the grid, retry.")
    }

    def grids(player: Player, gridSize: Int): Unit = {
        val boatSquares: List[Square] = player.boats.flatMap((b) => b.squares)

        println(player.name + " boat grid :")
        def displayGrid(x: Int, y: Int, gridSize: Int, boatSquares: List[Square]): Unit = {
            //Numbers on top
            if(x <= gridSize && y == gridSize+1){
                print(" " + x)
                displayGrid(x+1, y, gridSize, boatSquares)
            }
            //Last top square
            else if(x == gridSize+1 && y == gridSize+1){
                println()
                displayGrid(1, y-1, gridSize, boatSquares)
            }
            //Normal square
            else if(x <= gridSize && y > 0){
                val filteredSquares: List[Square] =  boatSquares.filter((s) => s.x == x && s.y == y)
                if(filteredSquares.length > 0){
                    val square: Square = filteredSquares.head
                    print("|" + square.state)
                }
                else{
                    print("| ")
                }
                displayGrid(x+1, y, gridSize, boatSquares)
            }
            //Numbers on right
            else if(x >= gridSize && y > 0 && y < gridSize+1){
                println("| " + y)
                displayGrid(1, y-1, gridSize, boatSquares)
            }
        }
        displayGrid(1, gridSize+1, gridSize, boatSquares)
    }
}