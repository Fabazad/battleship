
import helpers.DisplayHelper
import helpers.AskHelper
import players._
import boats.Boat
import game._
import grid._
import scala.util.Random
import java.io._
import scala.annotation.tailrec

object Main extends App {

    DisplayHelper.clear()
    mainLoop();

    /**
      *
      * @param game This is the game state, it contains the two players.
      * @param isContest Indicates if the game is part of a contest.
      * @return Returns the Option of the Winner (Player). None if the user quit before finish the game.
      */
    @tailrec
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
        else if(isContest || AskHelper.continuOrQuit(newPlayer2)) gameLoop(newGame, isContest)
        else None   
    }

    /**
      *
      * @param emptyPlayer1 The first AIPlayer to participate at the contest.
      * @param emptyPlayer2 The second AIPlayer to participate at the contest.
      * @param acc The number of game that remains (so the number of game of the contest.
      * @param bigContest Indicates if the contest is part of a big contest (with many AIs).
      * @return An object Contest that contains players with their score about the contest.
      */
    @tailrec
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

    /**
      * The Main loop of the App
      */
    @tailrec
    def mainLoop(){
        DisplayHelper.clear
        AskHelper.contestOrGame match {
            // Simple game between at least one User Player
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
            // Contest with two AIs
            case "2" => {
                val player1: Player = AskHelper.whichAI("AI player 1").askForBoats
                val player2: Player = AskHelper.whichAI("AI player 2").askForBoats

                contestLoop(player1, player2, GameSettings.contestGames, false)
                if(AskHelper.returnToMenu) mainLoop()
            }
            //Big contest with all AIs
            case "3" => {
                val file = new File("result.csv")
                val bw = new BufferedWriter(new FileWriter(file))

                // Create all players
                val player1: Player = AIPlayer("AI level 1", 1)
                val player2: Player = AIPlayer("AI level 2", 2)
                val player3: Player = AIPlayer("AI level 3", 3)

                //Play all the contest
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
            // Quit
            case "4" => {
                println("Good bye")
            }
        }
    }
}