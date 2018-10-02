
import helpers.DisplayHelper
import helpers.AskHelper
import players.UserPlayer
import players.Player
import boats.Boat
import game.Game
import game.GameSettings

object Main extends App {
    DisplayHelper.rules()

    val emptyPlayer1: Player = UserPlayer("Player 1")
    val emptyPlayer2: Player = UserPlayer("Player 2")

    val player1: Player = emptyPlayer1.askForBoats()
    val player2: Player = emptyPlayer2.askForBoats()

    val game: Game = Game(player1, player2, 1)
    val gs: GameSettings = GameSettings()

    mainLoop(game, gs)

    def mainLoop(game: Game, gs: GameSettings){
        println(gs)
    }
}