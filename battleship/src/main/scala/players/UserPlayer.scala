package players

import boats._
import helpers._
import game.GameSettings
import scala.util.Random
import grid._

/**
  * The class that represent a Player, but more precisely the player as user.
  * @param name Name of the player.
  * @param boats The ships of the player.
  * @param sentShots The sent shots by the player during a game.
  * @param receivedShots The received shots by the player during a game.
  * @param score The score of a player during a contest.
  */
case class UserPlayer(
    override val name: String, 
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List(),
    override val score: Int = 0
) 
extends Player(name, boats, sentShots, receivedShots){

    override def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
        
        DisplayHelper.displayGrid(otherBoats, List())

        val headx: Int = AskHelper.boatHeadX(size)
        val heady: Int = AskHelper.boatHeadY(size)
        val direction: String = AskHelper.boatDirection(size)

        val boat: Boat = Boat(size, headx, heady, direction)

        // Check Boat
        if(boat.isOutGrid){
            DisplayHelper.boatOutGrid
            askForBoat(otherBoats, size)
        }
        else if (boat.isCrossingBoat(otherBoats)){
            DisplayHelper.errorCrossingBoat()
            askForBoat(otherBoats, size)
        }
        else boat
    }

    override def addSentShot(shot: Shot): Player = {
        new UserPlayer(name, boats, shot::sentShots, receivedShots, score)
    }

    override def addReceivedShot(shot: Shot): Player = {
        new UserPlayer(name, boats, sentShots, shot::receivedShots, score)
    }

    override def askForShot(): Shot = {
        val x: Int = AskHelper.shotx()
        val y: Int = AskHelper.shoty()

        val shot: Shot = Shot(Pos(x,y))

        //Check shots
        if(shot.isOutGrid){
            DisplayHelper.shotOutGrid()
            askForShot()
        }
        else if(shot.isAlreadyShot(sentShots)){
            DisplayHelper.alreadyShot()
            askForShot()
        }
        else shot
    }

    override def addScore(): UserPlayer = {
        UserPlayer(name, boats, sentShots, receivedShots, score+1)
    }
}

object UserPlayer {
    /**
      * Create a new empty user player.
      * @param name The name of the player.
      * @return The empty player, just a name.
      */
    def apply(name: String): UserPlayer = {
        new UserPlayer(name)
    }
}