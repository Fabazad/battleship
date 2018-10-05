package players

import boats._
import helpers._
import game.GameSettings
import scala.util.Random
import grid._

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
        val shot: Shot = Shot(Pos(x,y))
        if(shot.isOutGrid){
            DisplayHelper.shotOutGrid()
            askForShot()
        }
        else if(shot.isAlreadyShot(sentShots)){
            DisplayHelper.alreadyShot()
            askForShot()
        }
        else{
            shot
        }
    }
}

object UserPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }
}