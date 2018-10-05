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
                val lastTouchedShotOption: Option[Shot] = Shot.lastTouchedShot(sentShots)
                lastTouchedShotOption match {
                    case None =>{
                        Shot.randomShot(sentShots)
                    }
                    case Some(lastTouchedShot) => {
                        if(lastTouchedShot.sankBoat) Shot.randomShot(sentShots)
                        else{
                            val firstTouchedShotOption: Option[Shot] = Shot.firstTouchedShot(sentShots)
                            val firstTouchedShot = firstTouchedShotOption.get
                            if(firstTouchedShot.pos.equal(lastTouchedShot.pos)){
                                Shot.shotAround(firstTouchedShot, sentShots).getOrElse(Shot.randomShot(sentShots))
                            }
                            else{
                                Shot.shotNear(firstTouchedShot, lastTouchedShot, sentShots).getOrElse(Shot.randomShot(sentShots))
                            }
                        }
                    }
                }
            }
            case _ => {
                Shot.randomShot(sentShots)
            }
        }
        DisplayHelper.shotThere(shot, this)
        shot
    }
}

object AIPlayer {
    def apply(name: String): AIPlayer = {
        new AIPlayer(name)
    }
}