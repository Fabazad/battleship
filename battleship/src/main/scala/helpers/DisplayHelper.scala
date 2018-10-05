package helpers

import players._
import game.GameSettings
import boats._
import grid._

object DisplayHelper {
    def rules(): Unit = {
        println("Ici on écris les règles")
    } 

    def boatOutGrid(): Unit = {
        println("The boat is out of the grid. Retry.")
    }

    def errorCrossingBoat(): Unit = {
        println("The boat is crossing another. Retry.")
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
        println("The shot is out of the grid. Retry.")
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
        println("You already shot this position. Retry.")
    }

    def grids(player: Player): Unit = {
        val gridSize: Int = GameSettings.gridSize
        val boats: List[Boat] = player.boats

        println(player.name + " boat grid :")
        displayGrid(boats, player.receivedShots)
        println(player.name + " shots grid :")
        displayGrid(List(), player.sentShots)
    }

    def shotThere(shot: Shot, player: Player): Unit = {
        println("Player " + player.name + " shot on (" + shot.pos.x + "," + shot.pos.y + ").")
    }

    def displayGrid(boats: List[Boat], shots: List[Shot]): Unit = {
        val boatCells: List[Cell] = boats.flatMap((b) => b.cells) 
        displayGridBis(1, GameSettings.gridSize+1, GameSettings.gridSize, boatCells, shots)
    }
    def displayGridBis(x: Int, y: Int, gridSize: Int, boatCells: List[Cell], shots: List[Shot]): Unit = {
        //Numbers on top
        if(x <= gridSize && y == gridSize+1){
            print(" " + x)
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
                print("|" + cell.state)
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
            println("| " + y)
            displayGridBis(1, y-1, gridSize, boatCells, shots)
        }
    }

    def score(player1: Player, player2: Player): Unit = {
        println(player1.name + " : " + player1.score)
        println(player2.name + " : " + player2.score)
    }

    def clear(): Unit = {
        print("\033[H\033[2J")
    }
}