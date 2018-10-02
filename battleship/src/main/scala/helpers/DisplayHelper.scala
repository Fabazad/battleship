package helpers

import players.Player
import game.GameSettings

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

    def grids(player: Player, gridSize: Int): Unit = {
        println(player.name + " boat grid :")
        def displayGrid(x: Int, y: Int, gridSize: Int): Unit = {
            //Numbers on top
            if(x <= gridSize && y == gridSize+1){
                print(" " + x)
                displayGrid(x+1, y, gridSize)
            }
            //Numbers on right
            else if(x == gridSize+1 && y == gridSize+1){
                println()
                displayGrid(1,y-1, gridSize)
            }
            //Normal square
            else if(x < gridSize && y > 0){
                print("| ")
                displayGrid(x+1, y, gridSize)
            }
            //Last line square
            else if(x >= gridSize && y > 0 && y < gridSize+1){
                println("| | " + y)
                displayGrid(1, y-1, gridSize)
            }
        }
        displayGrid(1, gridSize+1, gridSize)
    }
}