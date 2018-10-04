
import helpers.DisplayHelper
import helpers.AskHelper
import players._
import boats.Boat
import game.Game
import game.GameSettings

object Main extends App {
    DisplayHelper.clear()
    DisplayHelper.rules()

    val emptyPlayer1: Player = UserPlayer("Player 1")
    val emptyPlayer2: Player = UserPlayer("Player 2")

    val player1: Player = emptyPlayer1.askForBoats()
    AskHelper.nextPlayer(emptyPlayer2)
    val player2: Player = emptyPlayer2.askForBoats()

    val game: Game = Game(player1, player2)

    mainLoop(game)

    def mainLoop(game: Game){
        DisplayHelper.clear()
        DisplayHelper.playerTurn(game.player1.name)
        DisplayHelper.grids(game.player1)
        DisplayHelper.playerTurn(game.player1.name)

        val shot: Shot = game.player1.shot(game.player2)

        val newPlayer1: Player = game.player1.addSentShot(shot)
        val newPlayer2: Player = game.player2.addReceivedShot(shot)
        
        val newGame: Game = Game(newPlayer2, newPlayer1)
        
        if(newPlayer2.lose()){
            DisplayHelper.playerWin(newPlayer1.name)
        }
        else if(AskHelper.continuOrQuit(newPlayer2) != "Q") mainLoop(newGame)
        
    }
}