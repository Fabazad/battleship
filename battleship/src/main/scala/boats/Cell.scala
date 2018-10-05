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

object Cell{
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