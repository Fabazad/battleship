
import helpers.DisplayHelper
import helpers.AskHelper
import players._
import boats.Boat
import game._
import grid._
import scala.util.Random

object Main extends App {
    DisplayHelper.clear()

    AskHelper.contestOrGame match {
        case "G" => {
            DisplayHelper.rules()

            val emptyPlayer1: Player = UserPlayer("Player 1")
            val emptyPlayer2: Player = AskHelper.userOrAI("Player 2")

            val player1: Player = emptyPlayer1.askForBoats()
            AskHelper.nextPlayer(emptyPlayer2)
            val player2: Player = emptyPlayer2.askForBoats()
            

            val game: Game = Game(player1, player2)

            gameLoop(game, false)
        }
        case "C" => {
            val player1: Player = AskHelper.whichAI("AI player 1").askForBoats
            val player2: Player = AskHelper.whichAI("AI player 2").askForBoats

            contestLoop(player1, player2, 1000)
        }
    }

    def gameLoop(game: Game, isContest: Boolean): Option[Player] = {
        DisplayHelper.clear()
        if(!isContest)DisplayHelper.playerTurn(game.player1)

        val shot: Shot = game.player1.shot(game.player2)

        val newPlayer1: Player = game.player1.addSentShot(shot)
        val newPlayer2: Player = game.player2.addReceivedShot(shot)
        
        val newGame: Game = Game(newPlayer2, newPlayer1)
        
        if(newPlayer2.lose()){
            DisplayHelper.playerWin(newPlayer1.name)
            Some(newPlayer1)
        }
        else if(isContest || AskHelper.continuOrQuit(newPlayer2) != "Q") gameLoop(newGame, isContest)
        else None   
    }

    def contestLoop(initPlayer1: Player, initPlayer2: Player, acc: Int){
        acc match {
            case 0 => {
                DisplayHelper.clear
                DisplayHelper.score(initPlayer1, initPlayer2)
            } 
            case _ => {
                val player1: Player = initPlayer1.askForBoats()
                val player2: Player = initPlayer2.askForBoats()

                val game = Game(player1, player2)
                val winner = gameLoop(game, true).get.addScore
                if(winner.equal(player1)) contestLoop(player2, winner, acc-1)
                else contestLoop(player1, winner, acc-1)
            }
        }
    }
}