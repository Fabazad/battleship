package players

import boats.Boat
import helpers._
import game.GameSettings

case class UserPlayer(override val name: String, override val boats: List[Boat] = List()) extends Player(name, boats){

    override def askForBoats(gs: GameSettings): UserPlayer = {
        DisplayHelper.playerTurn(name)
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int], gs: GameSettings): UserPlayer = {
            remainingBoats match {
                case Nil => new UserPlayer(name, otherBoats)
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
        if (boat.isCrossingBoat(otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size, gs)
        }
        else{
            boat
        }
    }

    override def shot(target: Player, gs: GameSettings): Shot = {
        UserPlayer.askForShot(gs)
    }
}

object UserPlayer {
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }

    def askForShot(gs: GameSettings): Shot = {
        val x: Int = AskHelper.shotx(gs)
        val y: Int = AskHelper.shoty(gs)
        if(x < 0 || x > gs.gridSize || y < 0 || y > gs.gridSize){
            DisplayHelper.shotOutGrid()
            askForShot(gs)
        }else{
            Shot(x, y, false)
        }
    }
}