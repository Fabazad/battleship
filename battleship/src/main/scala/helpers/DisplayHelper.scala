package helpers

import players._
import game._
import board._
import scala.annotation.tailrec

/**
  * The Helper object that contains all the methods that display text on the terminal for the user..
  */
object DisplayHelper {

    /**
      * Display the rules of the game.
      */
    def rules(): Unit = {
        println("\u001b[32m")
        println("Welcome to battleship Game !\n")
        println("Each player has " + GameSettings.boats.size + " ships of sizes " + GameSettings.boats + ".")
        println("They have to place this ships on a grid of size " + GameSettings.gridSize + "x" + GameSettings.gridSize + ".")
        println("Each turn the players can shot on a position in order to sink the other player ships.")
        println("The first to sink all the opponent ships win the game.")
        println("You can play agains an AI, AI level 1 is easy and the one level 3 is hard.")
        println("Have a good game !")
        println("\u001b[0m")
    }

    /**
      * Indicates that the boat is outside the grid.
      */
    def boatOutGrid(): Unit = {
        println("\u001b[33mThe ship is out of the grid. Retry.\u001b[0m")
    }

    /**
      * Indicates that the last ship entred is crossing another one.
      */
    def errorCrossingBoat(): Unit = {
        println("\u001b[33mThe shot is crossing another one. Retry.\u001b[0m")
    }

    /**
      * Indicates with player turn is.
      * @param player The player whose turn is.
      */
    def playerTurn(player: Player): Unit = {
        player match {
            case UserPlayer(n,b,ss,rs,s) => {
                println(player.name + " turn :")
                grids(player)
            }
            case AIPlayer(n,l,b,ss,rs,s) => println(player.name + " is playing.")
        }
    }

    /**
      * Indicates that the entered shot is outside grid.
      */
    def shotOutGrid(): Unit = {
        println("\u001b[33mThe shot is out of the grid. Retry.\u001b[0m")
    }

    /**
      * Indicates that the last shot succeed.
      */
    def shotSuccess(): Unit = {
        println("The shot succeed !")
    }

    /**
      * Indicate that the last shot failed.
      */
    def shotFailure(): Unit = {
        println("The shot failed !")
    }

    /**
      * Indicate tha the last ship sank.
      * @param boat The ship which have sunk.
      */
    def sunkBoat(boat: Boat): Unit = {
        println("Ship of size " + boat.size() + " has been sank !")
    }

    /**
      * Indicates that a player have won a game.
      * @param playerName The player who won.
      */
    def playerWin(playerName: String): Unit = {
        println(playerName + " win the game !")
    }

    /**
      * Indicates that the last shot was already shot at the same location.
      */
    def alreadyShot(): Unit = {
        println("\u001b[33mYou already shot this position. Retry.\u001b[0m")
    }

    /**
      * Display the two grids of a player (The boat and received shots grid but also the sent shots grid)
      * @param player The player whose grid is displayed.
      */
    def grids(player: Player): Unit = {
        val gridSize: Int = GameSettings.gridSize
        val boats: List[Boat] = player.boats

        println(player.name + " boat grid :")
        displayGrid(boats, player.receivedShots)
        println(player.name + " shot grid :")
        displayGrid(List(), player.sentShots)
    }

    /**
      * Display where the player just have shot.
      * @param shot The shot that we display.
      * @param player The player who has shot.
      */
    def shotThere(shot: Shot, player: Player): Unit = {
        println("Player " + player.name + " shot on (" + shot.pos.x + "," + shot.pos.y + ").")
    }

    /**
      * Display a grid with some ships and shots.
      * @param boats The ships to display.
      * @param shots The shots to display.
      */
    def displayGrid(boats: List[Boat], shots: List[Shot]): Unit = {
        val boatCells: List[Cell] = boats.flatMap((b) => b.cells) 
        displayGridBis(1, GameSettings.gridSize+1, GameSettings.gridSize, boatCells, shots)
        @tailrec
        def displayGridBis(x: Int, y: Int, gridSize: Int, boatCells: List[Cell], shots: List[Shot]): Unit = {
            //Numbers on top
            if(x <= gridSize && y == gridSize+1){
                print(" \u001b[36m" + x + "\u001b[0m")
                displayGridBis(x+1, y, gridSize, boatCells, shots)
            }
            //Last top Cell
            else if(x == gridSize+1 && y == gridSize+1){
                println()
                displayGridBis(1, y-1, gridSize, boatCells, shots)
            }
            //Normal Cell
            else if(x <= gridSize && y > 0){
                val filteredCells: List[Cell] =  boatCells.filter((s) => s.pos.equal(Pos(x,y)))
                val filteredShots: List[Shot] =  shots.filter((rs) => rs.pos.equal(Pos(x,y)))
                
                if(filteredCells.length > 0 && filteredShots.length < 1){
                    val cell: Cell = filteredCells.head
                    print("|\u001b[33m" + cell.state + "\u001b[0m")
                }
                else if(filteredShots.length > 0){
                    if(filteredShots.head.touched)
                        print("|" + GameSettings.touchedDisplay)
                    else
                        print("|" + GameSettings.untouchedDisplay)
                }
                else{
                    print("| ")
                }
                displayGridBis(x+1, y, gridSize, boatCells, shots)
            }
            //Numbers on right
            else if(x >= gridSize && y > 0 && y < gridSize+1){
                println("| \u001b[35m" + y + "\u001b[0m")
                displayGridBis(1, y-1, gridSize, boatCells, shots)
            }
        }
    }

    /**
      * Display the score of 2 AI from a contest.
      * @param player1 The first player of the contest.
      * @param player2 The second player of the contest
      */
    def score(player1: Player, player2: Player): Unit = {
        if(player1.score > player2.score) print("\u001b[32m")
        else print("\u001b[31m")
        println(player1.name + " : " + player1.score)
        if(player1.score > player2.score) print("\u001b[31m")
        else print("\u001b[32m")
        print(player2.name + " : " + player2.score)
        println("\u001b[0m")
    }

    /**
      * Clear the console.
      */
    def clear(): Unit = {
        print("\033[H\033[2J")
    }

    /**
      * Write a ligne for the csv file with a contest object.
      * @param contest The contest from which one the function will write a csv file line.
      * @return The line (string) of the contest score.
      */
    def csvLine(contest: Contest): String = {
        val aiPlayer1 : AIPlayer = contest.player1 match { case AIPlayer(n,l,b,rs,ss,s) => AIPlayer(n,l,b,rs,ss,s)}
        val aiPlayer2 : AIPlayer = contest.player2 match { case AIPlayer(n,l,b,rs,ss,s) => AIPlayer(n,l,b,rs,ss,s)}
        
        val player1: Player = if(aiPlayer1.level > aiPlayer2.level) aiPlayer2 else aiPlayer1
        val player2: Player = if(aiPlayer1.level > aiPlayer2.level) aiPlayer1 else aiPlayer2
        player1.name + ";" + player1.score + ";" + player2.name + ";" + player2.score + "\n"
    }

    /**
     * Indicates the end of a big contest between all AIs and where to find the csv file.
     */
    def endOfBigContest(): Unit = {
        println("End of the big contest, please find csv at result.csv.")
    }
}