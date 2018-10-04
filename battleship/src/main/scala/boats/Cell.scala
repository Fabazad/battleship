package boats

import game._
import players.Shot

case class Cell(val x: Int, val y: Int, val state: String) {

    def changeState(newState: String): Cell = {
        Cell(x, y, newState)
    }

    def isTouched(shots: List[Shot]): Boolean = {
        shots.filter((s) => s.x == x && s.y == y).length > 0
    }
    
}