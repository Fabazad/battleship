package boats

import game.GameSettings
import helpers.DisplayHelper
import players._
import grid._

case class Boat(cells: List[Cell]) {
    def isCrossingBoat(otherBoats: List[Boat]): Boolean = {
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
}

object Boat {
    def apply(size: Int, headx: Int, heady: Int, direction: String): Option[Boat] = {
        val gridSize: Int = GameSettings.gridSize
        val cells: List[Cell] = createCellList(size, size, direction, headx, heady)
        val cellOut: Boolean = cells.filter((c) => c.isOutGrid).length > 0
        if(cellOut){
            DisplayHelper.boatOutGrid()
            None 
        }else{
            Some(new Boat(cells))
        } 
    }

    def createCellList(acc: Int, size: Int, direction: String, headx: Int, heady: Int): List[Cell] = {
        acc match {
            case 0 => Nil
            case _ => { 
                direction match {
                    case "T" => new Cell(Pos(headx, heady), size.toString)::(createCellList(acc-1, size, direction, headx, heady+1))
                    case "B" => new Cell(Pos(headx, heady), size.toString)::(createCellList(acc-1, size, direction, headx, heady-1))
                    case "L" => new Cell(Pos(headx, heady), size.toString)::(createCellList(acc-1, size, direction, headx-1, heady))
                    case "R" => new Cell(Pos(headx, heady), size.toString)::(createCellList(acc-1, size, direction, headx+1, heady))
                }
            }
        }
    }
}