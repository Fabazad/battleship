
import helpers.DisplayHelper
import players.AIPlayer
import boats.Boat

object Main extends App {
    val boat: Boat = Boat(5, 4, 4, "T").get
    val boats: Array[Boat] = Array(boat)
    val player: AIPlayer = new AIPlayer(boats)
    println(boat)
    DisplayHelper.rules()
    DisplayHelper.grids(player)
}