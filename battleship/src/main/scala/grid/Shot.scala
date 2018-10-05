package grid

import boats._
import players._
import game._
import scala.util.Random
import grid._

case class Shot(
    val pos: Pos,
    val touched: Boolean = false, 
    val boatSize: Int = 0, 
    val sankBoat: Boolean = false
){
    def setTouched(newTouched: Boolean): Shot = {
        Shot(pos, newTouched, boatSize, sankBoat)
    }

    def setBoatSize(newBoatSize: Int): Shot = {
        Shot(pos, touched, newBoatSize, sankBoat)
    }

    def setSankBoat(newSankBoat: Boolean): Shot = {
        Shot(pos, touched, boatSize, newSankBoat)
    }

    def isOutGrid(): Boolean = {
        pos.isOutGrid
    }

    def isAlreadyShot(shots: List[Shot]): Boolean = {
        shots.filter((ss) => ss.pos.equal(pos)).length > 0
    }

    def isValid(shots: List[Shot]): Boolean = {
        !isOutGrid && !isAlreadyShot(shots)
    }
}

object Shot{
    def shotedBoat(boats: List[Boat], shot: Shot): Option[Boat] = {
        val shotedBoats: List[Boat] = boats.filter((b) => {
            b.cells.filter((c) => c.pos.equal(shot.pos)).length > 0
        })

        if(shotedBoats.isEmpty) None else Some(shotedBoats.head)
    }

    def randomShot(shots: List[Shot]): Shot = {
        val r: Random = Game.random
        val remainingPos: List[Pos] = Pos.allPos.filter((p) => !p.alreadyShot(shots))
        val randomPos: Pos = remainingPos(r.nextInt(remainingPos.length))
        Shot(randomPos)
    }

    def shotAround(shot: Shot, shots: List[Shot]): Shot = {
        Shot(Pos(1,1))
    }

    def randomShotAround(shot: Shot, shots: List[Shot]): Shot = {
        val r: Random = Game.random
        val dir: Int = r.nextInt(4)
        val newShot: Shot = dir match {
            case 0 => Shot(shot.pos.top)
            case 1 => Shot(shot.pos.right)
            case 2 => Shot(shot.pos.bottom)
            case 3 => Shot(shot.pos.left)
        }
        if(newShot.isOutGrid || newShot.isAlreadyShot(shots)){
            randomShotAround(shot, shots)
        } 
        else newShot
    }

    def isValidShot(x: Int, y: Int, shots: List[Shot]): Boolean = {
        Shot(Pos(x,y)).isValid(shots)
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