package boats

import game.GameSettings
import helpers.DisplayHelper
import players.Player

case class Boat(squares: List[Square]) {
}

object Boat {
    def apply(size: Int, headx: Int, heady: Int, direction: String): Option[Boat] = {
        val squares: List[Square] = createSquareList(size, direction, headx, heady)
        val squareOut: Boolean = squares.filter((s) => s.x < 0 || s.y < 0 || s.x > GameSettings.gridSize || s.y > GameSettings.gridSize).length > 0
        if(squareOut){
            DisplayHelper.boatOutGrid()
            None 
        }else{
            Some(new Boat(squares))
        } 
    }

    def createSquareList(size: Int, direction: String, headx: Int, heady: Int): List[Square] = {
        size match {
            case 0 => Nil
            case _ => { 
                direction match {
                    case "T" => new Square(headx, heady, false)::(createSquareList(size-1, direction, headx, heady-1))
                    case "B" => new Square(headx, heady, false)::(createSquareList(size-1, direction, headx, heady+1))
                    case "L" => new Square(headx, heady, false)::(createSquareList(size-1, direction, headx-1, heady))
                    case "R" => new Square(headx, heady, false)::(createSquareList(size-1, direction, headx+1, heady))
                }
            }
        }
    }

    def isCrossingBoat(boat: Boat, otherBoats: List[Boat]): Boolean = {
        def isCrossingBoatBis(squares: List[Square], otherBoats: List[Boat]): Boolean = {
            squares match {
                case Nil => false
                case x::l => {
                    val squares = otherBoats.flatMap((b) => b.squares)
                    val crossingSquares = squares.filter((s) => s.x == x.x && s.y == x.y)
                    crossingSquares.length > 0 || isCrossingBoatBis(l, otherBoats)
                }
            }
        }
        isCrossingBoatBis(boat.squares, otherBoats)
    }
}