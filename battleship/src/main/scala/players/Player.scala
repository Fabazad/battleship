package players

import boats._
import helpers._
import scala.util.Random
import game.GameSettings
import grid._
import scala.annotation.tailrec

/**
  * The class Player which is extended by AIPlayer and UserPlayer.
  * @param name Name of the player.
  * @param boats Ships of the player.
  * @param sentShots All the sent shot of the player during a game.
  * @param receivedShots All the received shots of the player during a game.
  * @param score The score of the player during a contest.
  */
abstract class Player(val name: String, val boats: List[Boat], val sentShots: List[Shot], val receivedShots: List[Shot], val score: Int = 0) {

    /**
      * Ask for a boat (the position) to the user.
      * @param otherBoats The boats already entered by the player.
      * @param size The size of the boat that the user should enter.
      * @return The final valid Boat entered by the player.
      */
    def askForBoat(otherBoats: List[Boat], size: Int): Boat

    /**
      * Add a received shot to the player.
      * @param shot The shot to be added.
      * @return The same player with this new shot in his sent shots.
      */
    def addSentShot(shot: Shot): Player

    /**
      * Ask for a shot (positions) to the player.
      * @return The final valid shot entered by the player.
      */
    def askForShot(): Shot

    /**
      * Add a shot to the received shots of the player.
      * @param shot The shot to be added.
      * @return The new player with the shot added to his sent shots.
      */
    def addReceivedShot(shot: Shot): Player

    /**
      * Increment the score of the player by one.
      * @return The new player with the score incremented.
      */
    def addScore(): Player

    /**
      * Ask to the player his ships (position).
      * @return The player with his new ships.
      */
    def askForBoats(): Player = {
        @tailrec
        def askForBoatsBis(otherBoats: List[Boat], remainingBoats: List[Int]): Player = {
            remainingBoats match {
                //No more boat
                case Nil => {
                    this match {
                        case UserPlayer(n,b,ss,rs,s) => UserPlayer(n, otherBoats, List(), List(), s)
                        case AIPlayer(n,l,b,ss,rs,s) => AIPlayer(n, l, otherBoats, List(), List(), s)
                    }
                }
                //It remains boats o add
                case x::l => {
                    val boat: Boat = askForBoat(otherBoats, x)
                    askForBoatsBis(boat::otherBoats, l)
                }
            }
        }
        askForBoatsBis(List(), GameSettings.boats)
    }

    /**
      * Create a shot from this player on the targeted player.
      * @param target This is the targeted player.
      * @return The shot that result of it.
      */
    def shot(target: Player): Shot = {

        //Get the shot
        val shot: Shot = askForShot()

        //Check if the shot touched a boat
        val shotedBoat: Option[Boat] = Shot.shotedBoat(target.boats, shot)

        shotedBoat match {
            //If no shoted boat
            case None => {
                DisplayHelper.shotFailure()
                shot
            }
            //If the shot succeed
            case Some(b) => {
                DisplayHelper.shotSuccess()
                //The boat has been sank
                if(b.isSunk(shot::target.receivedShots)){
                   DisplayHelper.sunkBoat(b)
                   Shot(shot.pos, true, true) 
                }
                else Shot(shot.pos, true)
            }
        }
    }

    /**
      * Indicates if the player lost the game.
      * @return True if the player has no more not sank ships.
      */
    def lose(): Boolean = {
        boats.filter((b) => !b.isSunk(receivedShots)).length < 1
    }

    /**
      * Indicates if this player is equal to the player entered. If they have the same name.
      * @param player The player to compare with.
      * @return True if they have the same name else false
      */
    def equal(player: Player): Boolean = {
        name == player.name
    }

}