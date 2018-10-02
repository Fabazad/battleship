
import helpers.DisplayHelper
import helpers.AskHelper
import players.UserPlayer
import players.Player
import boats.Boat

object Main extends App {
    val boats: List[Boat] = Player.askForBoats()
    val player: Player = Player(boats)
    println(player.boats)
    DisplayHelper.rules()
    DisplayHelper.grids(player)
}