package helpers

import players._
import game._
import boats._
import grid._
import scala.annotation.tailrec

object DisplayHelper {
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

    def boatOutGrid(): Unit = {
        println("\u001b[33mThe ship is out of the grid. Retry.\u001b[0m")
    }

    def errorCrossingBoat(): Unit = {
        println("\u001b[33mThe shot is crossing another one. Retry.\u001b[0m")
    }

    def playerTurn(player: Player): Unit = {
        player match {
            case UserPlayer(n,b,ss,rs,s) => {
                println(player.name + " turn :")
                grids(player)
            }
            case AIPlayer(n,l,b,ss,rs,s) => println(player.name + " is playing.")
        }
    }

    def shotOutGrid(): Unit = {
        println("\u001b[33mThe shot is out of the grid. Retry.\u001b[0m")
    }

    def shotSuccess(): Unit = {
        println("The shot succeed !")
    }

    def shotFailure(): Unit = {
        println("The shot failed !")
    }

    def sunkBoat(boat: Boat): Unit = {
        println("Boat of size " + boat.size() + " has been sank !")
    }

    def playerWin(playerName: String): Unit = {
        println(playerName + " win the game !")
    }

    def alreadyShot(): Unit = {
        println("\u001b[33mYou already shot this position. Retry.\u001b[0m")
    }

    def grids(player: Player): Unit = {
        val gridSize: Int = GameSettings.gridSize
        val boats: List[Boat] = player.boats

        println(player.name + " boat grid :")
        displayGrid(boats, player.receivedShots)
        println(player.name + " shot grid :")
        displayGrid(List(), player.sentShots)
    }

    def shotThere(shot: Shot, player: Player): Unit = {
        println("Player " + player.name + " shot on (" + shot.pos.x + "," + shot.pos.y + ").")
    }

    def displayGrid(boats: List[Boat], shots: List[Shot]): Unit = {
        val boatCells: List[Cell] = boats.flatMap((b) => b.cells) 
        displayGridBis(1, GameSettings.gridSize+1, GameSettings.gridSize, boatCells, shots)
    }

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

    def score(player1: Player, player2: Player): Unit = {
        if(player1.score > player2.score) print("\u001b[32m")
        else print("\u001b[31m")
        println(player1.name + " : " + player1.score)
        if(player1.score > player2.score) print("\u001b[31m")
        else print("\u001b[32m")
        print(player2.name + " : " + player2.score)
        println("\u001b[0m")
    }

    def clear(): Unit = {
        print("\033[H\033[2J")
    }

    def csvLine(contest: Contest): String = {
        
        val aiPlayer1 : AIPlayer = contest.player1 match { case AIPlayer(n,l,b,rs,ss,s) => AIPlayer(n,l,b,rs,ss,s)}
        val aiPlayer2 : AIPlayer = contest.player2 match { case AIPlayer(n,l,b,rs,ss,s) => AIPlayer(n,l,b,rs,ss,s)}
        
        val player1: Player = if(aiPlayer1.level > aiPlayer2.level) aiPlayer2 else aiPlayer1
        val player2: Player = if(aiPlayer1.level > aiPlayer2.level) aiPlayer1 else aiPlayer2
        player1.name + ";" + player1.score + ";" + player2.name + ";" + player2.score + "\n"
    }

    def endOfBigContest(): Unit = {
        println("End of the big contest, please find csv at result.csv.")
    }
}