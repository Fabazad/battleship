package boats

import game._
import grid._

case class Cell(val pos: Pos, val state: String) {

    def changeState(newState: String): Cell = {
        Cell(pos, newState)
    }

    def isTouched(shots: List[Shot]): Boolean = {
        shots.filter((s) => s.pos.equal(pos)).length > 0
    }

    def isOutGrid(): Boolean = {
        pos.isOutGrid
    }
    
}