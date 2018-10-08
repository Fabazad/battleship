package boats

import game._
import grid._
import scala.annotation.tailrec

/**
  * Cell of a boat.
  * @param pos The position of the cell.
  * @param state The state of the cell (The size of the boat)
  */
case class Cell(val pos: Pos, val state: String) {

    /**
      * Edit the state of the cell by returning another one.
      * @param newState The new state of the cell.
      * @return A cell with a new state.
      */
    def changeState(newState: String): Cell = {
        Cell(pos, newState)
    }

    /**
      * Indicates if the cell has been touched by a opponent shot.
      * @param shots The shots to check with.
      * @return True if the cell has been touched else False.
      */
    def isTouched(shots: List[Shot]): Boolean = {
        shots.filter((s) => s.pos.equal(pos)).length > 0
    }

    /**
      * Indicates if a cell is out of the grid.
      * @return True if the cell is out else False.
      */
    def isOutGrid(): Boolean = {
        pos.isOutGrid
    }
    
}

object Cell{
    /**
      * Create a list of cell usefull for a boat.
      * @param size The size of the desired list.
      * @param direction The direction of the boat.
      * @param headx The x postion of the head of the boat.
      * @param heady The y postion of the head of the boat
      * @return The list of cells.
      */
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
                        case "L" => createCellListBis(acc-1, size, direction, headx-1, heady, newCell::res)
                        case "R" => createCellListBis(acc-1, size, direction, headx+1, heady, newCell::res)
                    }
                }
            }
        }
        createCellListBis(size, size, direction, headx, heady, List())
    }
}