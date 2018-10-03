package players

import boats._
import helpers._
import game.GameSettings

case class AIPlayer(
    override val name: String, 
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List()
)  
extends Player(name, boats, sentShots, receivedShots){

    override def askForBoats(): AIPlayer = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): AIPlayer = {
            remainingBoats match {
                case Nil => new AIPlayer(name, otherBoats)
                case x::l => {
                    val boat: Boat = askForBoat(otherBoats, x)
                    askForBoatsBis(boat::otherBoats, l)
                }
            }
        }
        askForBoatsBis(List(), GameSettings.boats)
    }

    override def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
        val headx: Int = AskHelper.boatHeadX(size)
        val heady: Int = AskHelper.boatHeadY(size)
        val direction: String = AskHelper.boatDirection(size)
        val boat: Boat = Boat(size, headx, heady, direction).getOrElse(askForBoat(otherBoats, size))
        if (boat.isCrossingBoat(otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size)
        }
        else{
            boat
        }
    }

    override def shot(target: Player): Shot = {
        new Shot(1,1,false)
    }

    override def addSentShot(shot: Shot): Player = {
        new AIPlayer(name, boats, shot::sentShots, receivedShots)
    }
    
    override def addReceivedShot(shot: Shot): AIPlayer = {
        def boatsAfterShot(boats: List[Boat], shot: Shot): List[Boat] = {
            boats match {
                case Nil => Nil
                case b::l => {
                    val newCells: List[Cell] = b.cells.map((c) => {
                        if(c.x == shot.x && c.y == shot.y) c.changeState(GameSettings.touchedDisplay) else c
                    })
                    Boat(newCells)::boatsAfterShot(l, shot)
                }
            }
        }
        val newBoats = boatsAfterShot(boats, shot)
        AIPlayer(name, newBoats, sentShots, shot::receivedShots)
    }
}

object AIPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }
}