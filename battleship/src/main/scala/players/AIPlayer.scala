package players

import boats.Boat
import helpers._
import game.GameSettings

case class AIPlayer(override val name: String, override val boats: List[Boat] = List()) extends Player(name, boats){

    override def askForBoats(gs: GameSettings): AIPlayer = {
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int], gs: GameSettings): AIPlayer = {
            remainingBoats match {
                case Nil => new AIPlayer(name, otherBoats)
                case x::l => {
                    val boat: Boat = askForBoat(otherBoats, x, gs)
                    askForBoatsBis(boat::otherBoats, l, gs)
                }
            }
        }
        askForBoatsBis(List(), gs.boats, gs)
    }

    override def askForBoat(otherBoats: List[Boat], size: Int, gs: GameSettings): Boat = {
        val headx: Int = AskHelper.boatHeadX(size)
        val heady: Int = AskHelper.boatHeadY(size)
        val direction: String = AskHelper.boatDirection(size)
        val boat: Boat = Boat(size, headx, heady, direction, gs).getOrElse(askForBoat(otherBoats, size, gs))
        if (Boat.isCrossingBoat(boat, otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size, gs)
        }
        else{
            boat
        }
    }
}

object AIPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }
}