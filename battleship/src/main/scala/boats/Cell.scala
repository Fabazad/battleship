package boats

import game._
import grid._
import scala.annotation.tailrec

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
    def createCellList(size: Int, direction: String, headx: Int, heady: Int): List[Cell] = {
        @tailrec
        def createCellListBis(acc: Int, size: Int, direction: String, headx: Int, heady: Int, res: List[Cell]): List[Cell] = {
            val newCell: Cell = Cell(Pos(headx, heady), size.toString)
            acc match {
                case 0 => res
                case _ => { 
                    direction match {
                        case "T" => createCellListBis(acc-1, size, direction, headx, heady+1, newCell::res)
                        case "B" => createCellListBis(acc-1, size, direction, headx, heady-1, newCell::res)
                        case "L" => createCellListBis(acc-1, size, direction, headx+1, heady, newCell::res)
                        case "R" => createCellListBis(acc-1, size, direction, headx-1, heady, newCell::res)
                    }
                }
            }
        }
        createCellListBis(size, size, direction, headx, heady, List())
    }
}