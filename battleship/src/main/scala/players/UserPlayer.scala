package players

import boats._
import helpers._
import game.GameSettings
import scala.util.Random

case class UserPlayer(
    override val name: String, 
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List()
) 
extends Player(name, boats, sentShots, receivedShots){

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

    override def addSentShot(shot: Shot): Player = {
        new UserPlayer(name, boats, shot::sentShots, receivedShots)
    }

    override def addReceivedShot(shot: Shot): Player = {
        new UserPlayer(name, boats, sentShots, shot::receivedShots)
    }

    override def askForShot(): Shot = {
        val x: Int = AskHelper.shotx()
        val y: Int = AskHelper.shoty()
        val gridSize = GameSettings.gridSize
        if(x < 1 || x > gridSize || y < 1 || y > gridSize){
            DisplayHelper.shotOutGrid()
            askForShot()
        }
        else if(sentShots.filter((ss) => ss.x == x && ss.y == y).length > 0){
            DisplayHelper.alreadyShot()
            askForShot()
        }
        else{
            Shot(x, y, false)
        }
    }
}

object UserPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }
}