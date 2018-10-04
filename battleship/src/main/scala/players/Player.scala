package players

import boats._
import helpers._
import scala.util.Random
import game.GameSettings

abstract class Player(val name: String, val boats: List[Boat], val sentShots: List[Shot], val receivedShots: List[Shot]) {

    def askForBoat(otherBoats: List[Boat], size: Int): Boat

    def addSentShot(shot: Shot): Player

    def askForShot(): Shot

    def addReceivedShot(shot: Shot): Player = {
        def boatsAfterShot(boats: List[Boat], shot: Shot): List[Boat] = {
            boats match {
                case Nil => Nil
                case b::l => {
                    val newCells: List[Cell] = b.cells.map((c) => {
                        if(c.x == shot.x && c.y == shot.y) c.changeState(GameSettings.touchedDisplay) else c
                    })
                    //If the boat has been sank
                    if(Cell.touchedNumber(newCells) == b.size() && Cell.touchedNumber(newCells) != Cell.touchedNumber(b.cells)){
                        DisplayHelper.sunkBoat(b)
                    }
                    Boat(newCells)::boatsAfterShot(l, shot)
                }
            }
        }
        val newBoats = boatsAfterShot(boats, shot)
        this match {
            case UserPlayer(n, b, ss, rs) => UserPlayer(name, newBoats, sentShots, shot::receivedShots)
            case AIPlayer(n, b, ss, rs) => AIPlayer(name, newBoats, sentShots, shot::receivedShots)
        }
    }

    def askForBoats(): Player = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): Player = {
            remainingBoats match {
                case Nil => {
                    this match {
                        case UserPlayer(a,b,c,d) => UserPlayer(name, otherBoats)
                        case AIPlayer(a,b,c,d) => AIPlayer(name, otherBoats)
                    }
                }
                case x::l => {
                    val boat: Boat = askForBoat(otherBoats, x)
                    askForBoatsBis(boat::otherBoats, l)
                }
            }
        }
        askForBoatsBis(List(), GameSettings.boats)
    }
    
    def shot(target: Player): Shot = {
        val shot: Shot = askForShot()
        val targetCells: List[Cell] = target.boats.flatMap((b) => b.cells)
        val touched: Boolean = targetCells.filter((c) => c.x == shot.x && c.y == shot.y).length > 0
        if(touched) DisplayHelper.shotSuccess() else DisplayHelper.shotFailure()
        shot.setTouched(touched)
    }

    def lose(): Boolean = {
        val allCells: List[Cell] = boats.flatMap((b) => b.cells)
        allCells.filter((c) => !c.isTouched()).length == 0
    }
}