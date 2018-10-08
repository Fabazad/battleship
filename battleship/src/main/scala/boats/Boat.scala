package boats

import game.GameSettings
import helpers.DisplayHelper
import players._
import grid._
import scala.annotation.tailrec

case class Boat(cells: List[Cell]) {

    
    def isCrossingBoat(otherBoats: List[Boat]): Boolean = {
        @tailrec
        def isCrossingBoatBis(cells: List[Cell], otherBoats: List[Boat]): Boolean = {
            cells match {
                case Nil => false
                case c::l => {
                    val otherCells = otherBoats.flatMap((b) => b.cells)
                    val crossingCells = otherCells.filter((oc) => oc.pos.equal(c.pos))
                    crossingCells.length > 0 || isCrossingBoatBis(l, otherBoats)
                }
            }
        }
        isCrossingBoatBis(cells, otherBoats)
    }

    def isSunk(shots: List[Shot]): Boolean = {
        cells.filter((c) => c.isTouched(shots)).length == size
    }

    def size(): Int = {
        cells.length
    }

    def isOutGrid(): Boolean = {
        val gridSize: Int = GameSettings.gridSize
        cells.filter((c) => c.isOutGrid).length > 0
    }
}

object Boat {
    def apply(size: Int, headx: Int, heady: Int, direction: String): Boat = {
        val gridSize: Int = GameSettings.gridSize
        val cells: List[Cell] = Cell.createCellList(size, direction, headx, heady)
        val cellOut: Boolean = cells.filter((c) => c.isOutGrid).length > 0
        Boat(cells)
    }
}

