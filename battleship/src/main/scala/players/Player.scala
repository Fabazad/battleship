package players

import boats._
import helpers._
import scala.util.Random
import game.GameSettings

abstract class Player(val name: String, val boats: List[Boat], val sentShots: List[Shot], val receivedShots: List[Shot]) {

    def askForBoat(otherBoats: List[Boat], size: Int): Boat

    def addSentShot(shot: Shot): Player

    def askForShot(): Shot

    def addReceivedShot(shot: Shot): Player

    def askForBoats(): Player = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): Player = {
            remainingBoats match {
                case Nil => {
                    this match {
                        case UserPlayer(n,b,ss,rs) => UserPlayer(name, otherBoats)
                        case AIPlayer(n,l,b,ss,rs) => AIPlayer(name, l, otherBoats)
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

        val shotedBoat: Option[Boat] = Shot.shotedBoat(target.boats, shot)

        shotedBoat match {
            case None => {
                DisplayHelper.shotFailure()
                Shot(shot.x, shot.y)
            }
            case Some(b) => {
                DisplayHelper.shotSuccess()
                if(b.isSunk(shot::target.receivedShots)){
                   DisplayHelper.sunkBoat(b)
                   Shot(shot.x, shot.y, true, b.size, true) 
                }
                else Shot(shot.x, shot.y, true, b.size)
            }
        }
    }

    def lose(): Boolean = {
        boats.filter((b) => !b.isSunk(receivedShots)).length < 1
    }
}