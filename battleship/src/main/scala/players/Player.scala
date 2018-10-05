package players

import boats._
import helpers._
import scala.util.Random
import game.GameSettings
import grid._

abstract class Player(val name: String, val boats: List[Boat], val sentShots: List[Shot], val receivedShots: List[Shot], val score: Int = 0) {

    def askForBoat(otherBoats: List[Boat], size: Int): Boat

    def addSentShot(shot: Shot): Player

    def askForShot(): Shot

    def addReceivedShot(shot: Shot): Player

    def addScore(): Player

    def askForBoats(): Player = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): Player = {
            remainingBoats match {
                case Nil => {
                    this match {
                        case UserPlayer(n,b,ss,rs,s) => UserPlayer(n, otherBoats, List(), List(), s)
                        case AIPlayer(n,l,b,ss,rs,s) => AIPlayer(n, l, otherBoats, List(), List(), s)
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
                Shot(shot.pos)
            }
            case Some(b) => {
                DisplayHelper.shotSuccess()
                if(b.isSunk(shot::target.receivedShots)){
                   DisplayHelper.sunkBoat(b)
                   Shot(shot.pos, true, b.size, true) 
                }
                else Shot(shot.pos, true, b.size)
            }
        }
    }

    def lose(): Boolean = {
        boats.filter((b) => !b.isSunk(receivedShots)).length < 1
    }

    def equal(player: Player): Boolean = {
        name == player.name
    }

    def init() : Player
}