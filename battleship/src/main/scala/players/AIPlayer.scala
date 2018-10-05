package players

import boats._
import helpers._
import game._
import scala.util.Random
import grid._

case class AIPlayer(
    override val name: String,
    val level: Int = 1,
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List(),
)  
extends Player(name, boats, sentShots, receivedShots){

    override def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
        val r: Random = Game.random
        val headx: Int = r.nextInt(GameSettings.gridSize)+1
        val heady: Int = r.nextInt(GameSettings.gridSize)+1
        val direction: String = r.nextInt(4)+1 match {
            case 1 => "T"
            case 2 => "R"
            case 3 => "B"
            case 4 => "L"
        }
        val boat: Boat = Boat(size, headx, heady, direction).getOrElse(askForBoat(otherBoats, size))
        if (boat.isCrossingBoat(otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size)
        }
        else{
            boat
        }
    }

    override def addSentShot(shot: Shot): Player = {
        new AIPlayer(name, level, boats, shot::sentShots, receivedShots)
    }

    override def addReceivedShot(shot: Shot): Player = {
        new AIPlayer(name, level, boats, sentShots, shot::receivedShots)
    }

    override def askForShot(): Shot = {
        val shot: Shot = level match {
            case 2 => {
                def findShotLevelTwo(sentShots: List[Shot], acc: Int): Shot = {
                    Shot(Pos(1,1))
                }
                findShotLevelTwo(sentShots, 4) 
            }
            case _ => {
                Shot.randomShot(sentShots)
            }
        }

        val gridSize = GameSettings.gridSize
        if(shot.isOutGrid){
            askForShot()
        }
        else if(shot.isAlreadyShot(sentShots)){
            askForShot()
        }
        else{
            DisplayHelper.shotThere(shot, this)
            shot
        }
    }
}

object AIPlayer {
    def apply(name: String): AIPlayer = {
        new AIPlayer(name)
    }
}