package board

import players._
import game._
import scala.util.Random
import scala.annotation.tailrec

/**
  * Correspond to a shot on a player (His ships).
  * @param pos The position of the shot (x,y).
  * @param touched Indicates if the shot touched a ship.
  * @param sankBoat Indicates if the shot has sunk the ship if touched.
  */
case class Shot(
    val pos: Pos,
    val touched: Boolean = false,
    val sankBoat: Boolean = false
){
    /**
      * Return the same shot but with a new param touched.
      * @param newTouched The new value of touched.
      * @return The shot with the new value of touched.
      */
    def setTouched(newTouched: Boolean): Shot = {
        Shot(pos, newTouched, sankBoat)
    }

    /**
      * Return the same shot but with a new value of sank boat.
      * @param newSankBoat The new value.
      * @return The shot with the new value of sank boat.
      */
    def setSankBoat(newSankBoat: Boolean): Shot = {
        Shot(pos, touched, newSankBoat)
    }

    /**
      * Indicates if the shot is out of the grid.
      * @return True if it's outside else False.
      */
    def isOutGrid(): Boolean = {
        pos.isOutGrid
    }

    /**
      * Indicates if the pos of the shot has been already shot.
      * @param shots List of shots where the function will check the actual shot is not already inside.
      * @return True if already shot then False.
      */
    def isAlreadyShot(shots: List[Shot]): Boolean = {
        shots.filter((ss) => ss.pos.equal(pos)).length > 0
    }

    /**
      * Indicates if a shot is valid. That means it hasnt been already shot and it is in the grid.
      * @param shots
      * @return
      */
    def isValid(shots: List[Shot]): Boolean = {
        !isOutGrid && !isAlreadyShot(shots)
    }
}

object Shot{
    /**
      * Find if a ship has been touched by the shot and which one.
      * @param boats Ships where we look at it.
      * @param shot The shot that we check.
      * @return The ship which has been shot or None
      */
    def shotedBoat(boats: List[Boat], shot: Shot): Option[Boat] = {
        val shotedBoats: List[Boat] = boats.filter((b) => {
            b.cells.filter((c) => c.pos.equal(shot.pos)).length > 0
        })
        if(shotedBoats.isEmpty) None else Some(shotedBoats.head)
    }

    /**
      * Create a random and valid shot.
      * @param shots The list to check to not have an already done shot.
      * @return The random and valid Shot.
      */
    def randomShot(shots: List[Shot]): Shot = {
        val r: Random = Game.random
        val remainingPos: List[Pos] = Pos.allPos.filter((p) => !p.alreadyShot(shots))
        val randomPos: Pos = remainingPos(r.nextInt(remainingPos.length))
        Shot(randomPos)
    }

    /**
      * Try to get a valid shot around another shot.
      * @param shot The shot where we want to find around.
      * @param shots List of shots which permit to not create two times the same shot.
      * @return A shot around the entered shot or None if it can't.
      */
    def shotAround(shot: Shot, shots: List[Shot]): Option[Shot] = {
        val r: Random = Game.random
        val availablePos: List[Pos] = Pos.posAround(shot.pos).filter((p) => !p.alreadyShot(shots))

        // Check if there is available pos around
        if(availablePos.length > 0){
            val randomPos: Pos = availablePos(r.nextInt(availablePos.length))
            Some(Shot(randomPos))
        }
        else None
    }

    /**
      * Indicates if positions x and y can create a valid shot (in the grid and not already used).
      * @param x position x of the shot.
      * @param y position y of the shot.
      * @param shots The list of the sho in order to not shot at the same position two times.
      * @return True if its a valid shot else False.
      */
    def isValidShot(x: Int, y: Int, shots: List[Shot]): Boolean = {
        Shot(Pos(x,y)).isValid(shots)
    }

    /**
      * Find the last touched shots (last in time).
      * @param shots Shots where we are looking inside.
      * @return The last (in time) touched shot if possible else None
      */
    @tailrec
    def lastTouchedShot(shots: List[Shot]): Option[Shot] = {
        shots match {
            case Nil => None
            case s::ls => if(s.touched) Some(s) else lastTouchedShot(ls)
        }
    }

    /**
      * Find the first (in time) touched (and chich didn't sansk a ship) shot after the last sunk ship shot or the first shot if doesnt have.
      * @param shots the shots we are searching in.
      * @return The first (in time) touched and not sanking shot of the last touched and not sunk ship.
      */
    def firstTouchedShot(shots: List[Shot]): Option[Shot] = {
        @tailrec
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

    /**
      * Find a shot in the same line and near of the two entered shot.
      * @param firstShot The first shot to shot near.
      * @param lastShot The second shot to shot near.
      * @param shots The shots that permit to not shot at the same position.
      * @return The shot which is on the same line and near the two entered shot. If it can't find then None.
      */
    def shotNear(firstShot: Shot, lastShot: Shot, shots: List[Shot]): Option[Shot] = {
        val r: Random = Game.random
        val allPos: List[Pos] = Pos.allPos

        //If on the same line
        if(firstShot.pos.x == lastShot.pos.x){
            val nearPos: List[Pos] = List(firstShot.pos.left, firstShot.pos.right, lastShot.pos.left, lastShot.pos.right)
            val availablePos: List[Pos] = nearPos.filter((p) => !p.alreadyShot(shots))
            
            if(availablePos.length > 0){
                val randomPos:Pos = availablePos(r.nextInt(availablePos.length))
                Some(Shot(randomPos))
            }
            else None
        }

        //If on the same column
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

    /**
      * Inside grid and valid Shot from random on a pair position of the grid.
      * @param shots Shots in order to get valid position of the shot.
      * @return A random and good placed shot.
      */
    def randomModuloShot(shots: List[Shot]): Shot = {
        val r: Random = Game.random
        val remainingPos: List[Pos] = Pos.allPos.filter((p) => !p.alreadyShot(shots) && (p.x+p.y)%2 == 0)
        
        if(remainingPos.length > 0){
            val randomPos: Pos = remainingPos(r.nextInt(remainingPos.length))
            Shot(randomPos)
        }
        else randomShot(shots)
        
    }
}