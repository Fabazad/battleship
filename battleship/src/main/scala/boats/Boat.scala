package boats

import game.GameSettings
import helpers.DisplayHelper
import players.Player

case class Boat(cells: List[Cell]) {
    def isCrossingBoat(otherBoats: List[Boat]): Boolean = {
        def isCrossingBoatBis(cells: List[Cell], otherBoats: List[Boat]): Boolean = {
            cells match {
                case Nil => false
                case c::l => {
                    val otherCells = otherBoats.flatMap((b) => b.cells)
                    val crossingCells = otherCells.filter((oc) => oc.x == c.x && oc.y == c.y)
                    crossingCells.length > 0 || isCrossingBoatBis(l, otherBoats)
                }
            }
        }
        isCrossingBoatBis(cells, otherBoats)
    }

    def isSunk(): Boolean = {
        cells.filter((c) => c.state == GameSettings.touchedDisplay).length == 0
    }

    def size(): Int = {
        cells.length
    }
}

object Boat {
    def apply(size: Int, headx: Int, heady: Int, direction: String): Option[Boat] = {
        val gridSize: Int = GameSettings.gridSize
        val cells: List[Cell] = createCellList(size, size, direction, headx, heady)
        val CellOut: Boolean = cells.filter((s) => s.x < 0 || s.y < 0 || s.x > gridSize || s.y > gridSize).length > 0
        if(CellOut){
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
                    case "T" => new Cell(headx, heady, size.toString)::(createCellList(acc-1, size, direction, headx, heady+1))
                    case "B" => new Cell(headx, heady, size.toString)::(createCellList(acc-1, size, direction, headx, heady-1))
                    case "L" => new Cell(headx, heady, size.toString)::(createCellList(acc-1, size, direction, headx-1, heady))
                    case "R" => new Cell(headx, heady, size.toString)::(createCellList(acc-1, size, direction, headx+1, heady))
                }
            }
        }
    }
}