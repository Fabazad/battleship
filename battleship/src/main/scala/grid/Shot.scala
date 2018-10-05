package grid

import boats._
import players._
import game._
import scala.util.Random
import grid._
import scala.util.Random

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

    def shotAround(shot: Shot, shots: List[Shot]): Option[Shot] = {
        val r: Random = Game.random
        val availablePos: List[Pos] = Pos.posAround(shot.pos).filter((p) => !p.alreadyShot(shots))
        if(availablePos.length > 0){
            val randomPos: Pos = availablePos(r.nextInt(availablePos.length))
            Some(Shot(randomPos))
        }
        else None
        
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

    def lastTouchedShot(shots: List[Shot]): Option[Shot] = {
        shots match {
            case Nil => None
            case s::ls => if(s.touched) Some(s) else lastTouchedShot(ls)
        }
    }

    def firstTouchedShot(shots: List[Shot]): Option[Shot] = {
        def firstTouchedShotBis(shots: List[Shot], firstShot: Option[Shot]): Option[Shot] = {
            shots match {
                case Nil => firstShot
                case s::ls =>{
                    if(s.sankBoat) firstShot
                    else if(s.touched) firstTouchedShotBis(ls, Some(s))
                    else firstTouchedShotBis(ls, firstShot)
                } 
            }
        }
        firstTouchedShotBis(shots, None)
    }

    def shotNear(firstShot: Shot, lastShot: Shot, shots: List[Shot]): Option[Shot] = {
        val r: Random = Game.random
        val allPos: List[Pos] = Pos.allPos
        //Ligne
        if(firstShot.pos.x == lastShot.pos.x){
            val nearPos: List[Pos] = List(firstShot.pos.left, firstShot.pos.right, lastShot.pos.left, lastShot.pos.right)
            val availablePos: List[Pos] = nearPos.filter((p) => !p.alreadyShot(shots))
            if(availablePos.length > 0){
                val randomPos:Pos = availablePos(r.nextInt(availablePos.length))
                Some(Shot(randomPos))
            }
            else None
        }
        else if(firstShot.pos.y == lastShot.pos.y){
            val nearPos: List[Pos] = List(firstShot.pos.top, firstShot.pos.bottom, lastShot.pos.top, lastShot.pos.bottom)
            val availablePos: List[Pos] = nearPos.filter((p) => !p.alreadyShot(shots))
            if(availablePos.length > 0){
                val randomPos:Pos = availablePos(r.nextInt(availablePos.length))
                Some(Shot(randomPos))
            }
            else None
        }
        else None
    }
}