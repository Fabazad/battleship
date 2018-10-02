package helpers

import players.Player

object DisplayHelper {
    def rules(): Unit = {
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
}