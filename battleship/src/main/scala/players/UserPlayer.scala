package players

import boats.Boat
import helpers._
import game.GameSettings

case class UserPlayer(override val name: String, override val boats: List[Boat] = List()) extends Player(name, boats){

    override def askForBoats(): UserPlayer = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): UserPlayer = {
            remainingBoats match {
                case Nil => new UserPlayer(name, otherBoats)
                case x::l => {
                    val boat: Boat = askForBoat(otherBoats, x)
                    askForBoatsBis(boat::otherBoats, l)
                }
            }
        }
        askForBoatsBis(List(), GameSettings.boats)
    }

    override def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
        val headx: Int = AskHelper.boatHeadX(size)
        val heady: Int = AskHelper.boatHeadY(size)
        val direction: String = AskHelper.boatDirection(size)
        val boat: Boat = Boat(size, headx, heady, direction).getOrElse(askForBoat(otherBoats, size))
        if (Boat.isCrossingBoat(boat, otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size)
        }
        else{
            boat
        }
    }
}

object UserPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }
}