package helpers

import players.Player

object DisplayHelper {
    def rules(): Unit = {
        println("Ici on écris les règles")
    } 

    def grids(player: Player): Unit = {
        println("User grid")
    }
}