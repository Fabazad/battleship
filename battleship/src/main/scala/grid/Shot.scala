package grid

import boats._
import players._
import game._
import scala.util.Random
import grid._

case class Shot(
    val x: Int, 
    val y: Int, 
    val touched: Boolean = false, 
    val boatSize: Int = 0, 
    val sankBoat: Boolean = false
){
    def setTouched(newTouched: Boolean): Shot = {
        Shot(x, y, newTouched, boatSize, sankBoat)
    }

    def setBoatSize(newBoatSize: Int): Shot = {
        Shot(x, y, touched, newBoatSize, sankBoat)
    }

    def setSankBoat(newSankBoat: Boolean): Shot = {
        Shot(x, y, touched, boatSize, newSankBoat)
    }

    def isOutGrid(): Boolean = {
        x < 1 && y < 1 && x > GameSettings.gridSize && y > GameSettings.gridSize
    }

    def isAlreadyShot(shots: List[Shot]): Boolean = {
        shots.filter((ss) => ss.x == x && ss.y == y).length > 0
    }

    def isValid(shots: List[Shot]): Boolean = {
        !isOutGrid && !isAlreadyShot(shots)
    }
}

object Shot{
    def shotedBoat(boats: List[Boat], shot: Shot): Option[Boat] = {
        val shotedBoats: List[Boat] = boats.filter((b) => {
            b.cells.filter((c) => c.x == shot.x && c.y == shot.y).length > 0
        })

        if(shotedBoats.isEmpty) None else Some(shotedBoats.head)
    }

    def randomShot(): Shot = {
        val r: Random = Game.random
        val x: Int = r.nextInt(GameSettings.gridSize)+1
        val y: Int = r.nextInt(GameSettings.gridSize)+1
        Shot(x,y)
    }

    def shotAround(shot: Shot, shots: List[Shot]): Shot = {
        shots match {
            case Nil => randomShotAround(shot, shots)
            case s::l => {
                if(s.x+1 == shot.x && s.y == shot.y && Shot.isValidShot(shot.x-1, shot.y, shots)) Shot(shot.x-1, shot.y)
                else if(s.x == shot.x && s.y+1 == shot.y && Shot.isValidShot(shot.x, shot.y-1, shots)) Shot(shot.x, shot.y-1)
                else if(s.x-1 == shot.x && s.y == shot.y && Shot.isValidShot(shot.x+1, shot.y, shots)) Shot(shot.x+1, shot.y)
                else if(s.x == shot.x && s.y-1 == shot.y && Shot.isValidShot(shot.x, shot.y+1, shots)) Shot(shot.x, shot.y+1)
                else randomShotAround(shot, shots)
            }
        }
    }

    def randomShotAround(shot: Shot, shots: List[Shot]): Shot = {
        val r: Random = Game.random
        val dir: Int = r.nextInt(4)
        val newShot: Shot = dir match {
            case 0 => Shot(shot.x+1, shot.y)
            case 1 => Shot(shot.x, shot.y+1)
            case 2 => Shot(shot.x-1, shot.y)
            case 3 => Shot(shot.x, shot.y-1)
        }
        if(newShot.isOutGrid || newShot.isAlreadyShot(shots)){
            randomShotAround(shot, shots)
        } 
        else newShot
    }

    def isValidShot(x: Int, y: Int, shots: List[Shot]): Boolean = {
        Shot(x,y).isValid(shots)
    }

    def lastUsefullTouchedShot(sentShots: List[Shot], acc: Int): Option[Shot] = {
        acc match {
            case 0 => None
            case _ => sentShots match {
                case Nil => None
                case s::ls => if(s.touched && !s.sankBoat) Some(s) else lastUsefullTouchedShot(ls, acc-1)
            }
        }
    }
}