package helpers

import players.Player
import game.GameSettings

object DisplayHelper {
    def rules(gs: GameSettings): Unit = {
        println("Ici on écris les règles")
    } 

    def grids(player: Player): Unit = {
        println("User grid")
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
}