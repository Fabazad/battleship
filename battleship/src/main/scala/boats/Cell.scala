package boats

import game._

case class Cell(val x: Int, val y: Int, val state: String) {

    def changeState(newState: String): Cell = {
        Cell(x, y, newState)
    }

    def isTouched(): Boolean = {
        state == GameSettings.touchedDisplay
    }
    
}

object Cell{
    def touchedNumber(cells: List[Cell]): Int = {
        cells.filter((c) => c.isTouched()).length
    }
}