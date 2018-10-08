
import helpers.DisplayHelper
import helpers.AskHelper
import players._
import boats.Boat
import game._
import grid._
import scala.util.Random
import java.io._

object Main extends App {
    DisplayHelper.clear()

    mainLoop();

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

    def contestLoop(emptyPlayer1: Player, emptyPlayer2: Player, acc: Int, bigContest: Boolean): Contest = {
        acc match {
            case 0 => {
                if(bigContest)
                    DisplayHelper.clear
                    DisplayHelper.score(emptyPlayer1, emptyPlayer2)
                Contest(emptyPlayer1,emptyPlayer2)
            } 
            case _ => {
                val player1: Player = emptyPlayer1.askForBoats()
                val player2: Player = emptyPlayer2.askForBoats()

                val game = Game(player1, player2)
                val winner = gameLoop(game, true).get.addScore

                if(winner.equal(player1)) contestLoop(player2, winner, acc-1, false)
                else contestLoop(player1, winner, acc-1, false)
            }
        }
    }

    def mainLoop(){
        DisplayHelper.clear
        AskHelper.contestOrGame match {
            case "1" => {
                DisplayHelper.rules()
                AskHelper.continue()

                val emptyPlayer1: Player = UserPlayer("Player 1")
                val emptyPlayer2: Player = AskHelper.userOrAI("Player 2")

                val player1: Player = emptyPlayer1.askForBoats()
                AskHelper.nextPlayer(emptyPlayer2)
                val player2: Player = emptyPlayer2.askForBoats()
                

                val game: Game = Game(player1, player2)

                gameLoop(game, false)

                if(AskHelper.returnToMenu) mainLoop()
            }
            case "2" => {
                val player1: Player = AskHelper.whichAI("AI player 1").askForBoats
                val player2: Player = AskHelper.whichAI("AI player 2").askForBoats
                contestLoop(player1, player2, GameSettings.contestGames, false)
                if(AskHelper.returnToMenu) mainLoop()
            }
            case "3" => {
                val file = new File("result.csv")
                val bw = new BufferedWriter(new FileWriter(file))

                val player1: Player = AIPlayer("AI level 1", 1)
                val player2: Player = AIPlayer("AI level 2", 2)
                val player3: Player = AIPlayer("AI level 3", 3)


                bw.write("AI Name; score; AI Name2; score2\n")
                val contest1: Contest = contestLoop(player1, player2, GameSettings.contestGames, true)
                bw.write(DisplayHelper.csvLine(contest1))
                val contest2: Contest = contestLoop(player1, player3, GameSettings.contestGames, true)
                bw.write(DisplayHelper.csvLine(contest2))
                val contest3: Contest = contestLoop(player2, player3, GameSettings.contestGames, true)
                bw.write(DisplayHelper.csvLine(contest3))

                bw.close();

                DisplayHelper.clear
                DisplayHelper.endOfBigContest
                if(AskHelper.returnToMenu) mainLoop()
            }
            case "4" => {
                println("Good bye")
            }
        }
    }
}