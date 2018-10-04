package players

import boats._
import helpers._
import game.GameSettings

case class UserPlayer(
    override val name: String, 
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List()
) 
extends Player(name, boats, sentShots, receivedShots){

    override def askForBoats: UserPlayer = {
        DisplayHelper.playerTurn(name)
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): UserPlayer = {
            remainingBoats match {
                case Nil => new UserPlayer(name, otherBoats)
                case x::l => {
                    DisplayHelper.clear()
                    val boat: Boat = askForBoat(otherBoats, x)
                    askForBoatsBis(boat::otherBoats, l)
                }
            }
        }
        askForBoatsBis(List(), GameSettings.boats)
    }

    override def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
        DisplayHelper.displayGrid(otherBoats, List())

        val headx: Int = AskHelper.boatHeadX(size)
        val heady: Int = AskHelper.boatHeadY(size)
        val direction: String = AskHelper.boatDirection(size)
        val boat: Boat = Boat(size, headx, heady, direction).getOrElse(askForBoat(otherBoats, size))
        if (boat.isCrossingBoat(otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size)
        }
        else boat
    }

    override def shot(target: Player): Shot = {
        val shot: Shot = UserPlayer.askForShot(target)
        val targetCells: List[Cell] = target.boats.flatMap((b) => b.cells)
        val touched: Boolean = targetCells.filter((c) => c.x == shot.x && c.y == shot.y).length > 0
        if(touched) DisplayHelper.shotSuccess() else DisplayHelper.shotFailure()
        shot.setTouched(touched)
    }

    override def addSentShot(shot: Shot): Player = {
        new UserPlayer(name, boats, shot::sentShots, receivedShots)
    }

    override def addReceivedShot(shot: Shot): UserPlayer = {
        def boatsAfterShot(boats: List[Boat], shot: Shot): List[Boat] = {
            boats match {
                case Nil => Nil
                case b::l => {
                    val newCells: List[Cell] = b.cells.map((c) => {
                        if(c.x == shot.x && c.y == shot.y){
                            c.changeState(GameSettings.touchedDisplay)
                        }else c
                    })
                    //If the boat has been sank
                    if(Cell.touchedNumber(newCells) == b.size() && Cell.touchedNumber(newCells) != Cell.touchedNumber(b.cells)){
                        DisplayHelper.sunkBoat(b)
                    }
                    Boat(newCells)::boatsAfterShot(l, shot)
                }
            }
        }
        val newBoats = boatsAfterShot(boats, shot)
        UserPlayer(name, newBoats, sentShots, shot::receivedShots)
    }
}

object UserPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }

    def askForShot(target: Player): Shot = {
        val x: Int = AskHelper.shotx()
        val y: Int = AskHelper.shoty()
        val gridSize = GameSettings.gridSize
        if(x < 0 || x > gridSize || y < 0 || y > gridSize){
            DisplayHelper.shotOutGrid()
            askForShot(target)
        }
        else if(target.receivedShots.filter((rs) => rs.x == x && rs.y == y).length > 0){
            DisplayHelper.alreadyShot()
            askForShot(target)
        }
        else{
            Shot(x, y, false)
        }
    }
}