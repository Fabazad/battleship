package grid

import game.GameSettings

case class Pos(val x: Int, val y: Int){
    def equal(pos: Pos): Boolean = {
        x == pos.x && y == pos.y
    }

    def isOutGrid(): Boolean = {
        x < 1 && y < 1 && x > GameSettings.gridSize && y > GameSettings.gridSize
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
}
