package boats

import game.GameSettings
import helpers.DisplayHelper
import players._
import grid._
import scala.annotation.tailrec

/**
  * The boat/ship of the battleship game.
  * @param cells The cells that contain the boat.
  */
case class Boat(cells: List[Cell]) {

    /**
      * Indicates if this boat is crossing another boat thqnks to list of boat.
      * @param otherBoats The other boats to check with.
      * @return True if the boat is crossing another one else False.
      */
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

    /**
      * Indicates if a boat has been sunk.
      * @param shots We check thanks to this shots.
      * @return True if the boat has been sunk else False.
      */
    def isSunk(shots: List[Shot]): Boolean = {
        cells.filter((c) => c.isTouched(shots)).length == size
    }

    /**
      * Get the size of the boat.
      * @return The size of the boat.
      */
    def size(): Int = {
        cells.length
    }

    /**
      * Indicates if at least a part of the boat is outside the grid.
      * @return True if the boat is outside else False
      */
    def isOutGrid(): Boolean = {
        val gridSize: Int = GameSettings.gridSize
        cells.filter((c) => c.isOutGrid).length > 0
    }
}

object Boat {
    /**
      * Create a boat thanks to the position of his head, his size and his direction.
      * @param size The size of the boat.
      * @param headx The x position of the head of the boat.
      * @param heady The y postion of the head of the boat.
      * @param direction The direction of the boat. Top, Right, Bottom or Left.
      * @return The new boat with this caracteristics.
      */
    def apply(size: Int, headx: Int, heady: Int, direction: String): Boat = {
        val gridSize: Int = GameSettings.gridSize
        val cells: List[Cell] = Cell.createCellList(size, direction, headx, heady)
        
        val cellOut: Boolean = cells.filter((c) => c.isOutGrid).length > 0
        Boat(cells)
    }
}

