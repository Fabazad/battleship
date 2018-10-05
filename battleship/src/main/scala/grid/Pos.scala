package grid

import game.GameSettings

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
        def allPosBis(x: Int, y: Int): List[Pos] = {
            if(x == 0 && y == 0) Nil
            else if(x > 0) Pos(x,y)::allPosBis(x-1, y)
            else allPosBis(GameSettings.gridSize, y-1)
            
        }
        allPosBis(GameSettings.gridSize, GameSettings.gridSize)
    }
}
