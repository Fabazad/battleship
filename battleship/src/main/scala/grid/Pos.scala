package grid

import game.GameSettings
import scala.annotation.tailrec

case class Pos(val x: Int, val y: Int){
    def equal(pos: Pos): Boolean = {
        x == pos.x && y == pos.y
    }

    def isOutGrid(): Boolean = {
        x < 1 || y < 1 || x > GameSettings.gridSize || y > GameSettings.gridSize
    }

    def top(): Pos = {
        Pos(x+1, y)
    }

    def right(): Pos = {
        Pos(x, y+1)
    }

    def bottom(): Pos = {
        Pos(x-1, y)
    }

    def left(): Pos = {
        Pos(x, y-1)
    }

    def alreadyShot(shots: List[Shot]): Boolean = {
        shots.filter((s) => s.pos.equal(this)).length > 0
    }
}

object Pos {
    def allPos(): List[Pos] = {
        @tailrec
        def allPosBis(x: Int, y: Int, res: List[Pos]): List[Pos] = {
            if(y == 0) res
            else if(x > 0) allPosBis(x-1, y, Pos(x,y)::res)
            else allPosBis(GameSettings.gridSize, y-1, res)
            
        }
        allPosBis(GameSettings.gridSize, GameSettings.gridSize, List())
    }

    def posAround(pos: Pos): List[Pos] = {
        List(pos.top, pos.right, pos.bottom, pos.left)
    }
}
