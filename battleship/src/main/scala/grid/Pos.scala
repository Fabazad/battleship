package grid

import game.GameSettings
import scala.annotation.tailrec

/**
  * The position into the grid.
  * @param x The x position of (x,y).
  * @param y The y position of (x,y).
  */
case class Pos(val x: Int, val y: Int){

    /**
      * Indicates if the two pos are equals (same x and y)
      * @param pos The other pos to compare.
      * @return True if they are the same.
      */
    def equal(pos: Pos): Boolean = {
        x == pos.x && y == pos.y
    }

    /**
      * Indicates if the pos is outside the grid.
      * @return True if it's outside else False.
      */
    def isOutGrid(): Boolean = {
        x < 1 || y < 1 || x > GameSettings.gridSize || y > GameSettings.gridSize
    }

    /**
      *
      * @return Return the pos on the top of this one.
      */
    def top(): Pos = {
        Pos(x+1, y)
    }

    /**
      *
      * @return Return the pos on the right of this one.
      */
    def right(): Pos = {
        Pos(x, y+1)
    }

    /**
      *
      * @return Return the pos on the bottom of this one.
      */
    def bottom(): Pos = {
        Pos(x-1, y)
    }

    /**
      *
      * @return Return the pos on the left of this one.
      */
    def left(): Pos = {
        Pos(x, y-1)
    }

    /**
      * Indicates if the pos is already shot, knowing that with a list of shot.
      * @param shots The list of shot where the function try to find the same pos.
      * @return True if the pos has been already shot.
      */
    def alreadyShot(shots: List[Shot]): Boolean = {
        shots.filter((s) => s.pos.equal(this)).length > 0
    }
}

object Pos {
    /**
      * Get all positions in a grid (10x10 = 100 positions)
      * @return All the positions in the grid as a list.
      */
    def allPos(): List[Pos] = {
        @tailrec
        def allPosBis(x: Int, y: Int, res: List[Pos]): List[Pos] = {
            if(y == 0) res
            else if(x > 0) allPosBis(x-1, y, Pos(x,y)::res)
            else allPosBis(GameSettings.gridSize, y-1, res)
            
        }
        allPosBis(GameSettings.gridSize, GameSettings.gridSize, List())
    }

    /**
      * Find all positions around another position.
      * @param pos The pos where we want to search around.
      * @return A list of pos, which are the pos around the entered pos.
      */
    def posAround(pos: Pos): List[Pos] = {
        List(pos.top, pos.right, pos.bottom, pos.left)
    }
}
