package players

import boats.Boat
import helpers.AskHelper
import helpers.DisplayHelper
import game.GameSettings

case class Player(val boats: List[Boat] = List()) {

}

object Player {

    def askForBoats(): List[Boat] = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): List[Boat] = {
            remainingBoats match {
                case Nil => otherBoats
                case x::l => {
                    val boat: Boat = askForBoat(otherBoats, x)
                    askForBoatsBis(boat::otherBoats, l)
                }
            }
        }
        askForBoatsBis(List(), GameSettings.boats)
    }

    def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
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