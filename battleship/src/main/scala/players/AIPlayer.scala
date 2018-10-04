package players

import boats._
import helpers._
import game._
import scala.util.Random

case class AIPlayer(
    override val name: String, 
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List()
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
        new AIPlayer(name, boats, shot::sentShots, receivedShots)
    }

    override def askForShot(): Shot = {
        val r: Random = Game.random
        val x: Int = r.nextInt(GameSettings.gridSize)+1
        val y: Int = r.nextInt(GameSettings.gridSize)+1
        val gridSize = GameSettings.gridSize
        if(x < 0 || x > gridSize || y < 0 || y > gridSize){
            askForShot()
        }
        else if(sentShots.filter((ss) => ss.x == x && ss.y == y).length > 0){
            askForShot()
        }
        else{
            Shot(x, y, false)
        }
    }
}

object AIPlayer {
    def apply(name: String): AIPlayer = {
        new AIPlayer(name)
    }
}