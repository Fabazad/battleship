
import helpers.DisplayHelper
import helpers.AskHelper
import players.UserPlayer
import players.Player
import boats.Boat
import game.Game
import game.GameSettings

object Main extends App {

    val gs: GameSettings = GameSettings()

    DisplayHelper.rules(gs)

    val emptyPlayer1: Player = UserPlayer("Player 1")
    val emptyPlayer2: Player = UserPlayer("Player 2")

    val player1: Player = emptyPlayer1.askForBoats(gs)
    val player2: Player = emptyPlayer2.askForBoats(gs)

    val game: Game = Game(player1, player2)

    mainLoop(game, gs)

    def mainLoop(game: Game, gs: GameSettings){
        val player: Player = game.player1
        DisplayHelper.grids(player, gs.gridSize)
    }
}