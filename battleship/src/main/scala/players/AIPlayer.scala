package players

import boats._
import helpers._
import game._
import scala.util.Random
import grid._
import scala.annotation.tailrec

/**
  * The AI player that can play agains an user or agains another AI player.
  * @param name
  * @param level Level of the AI (1: Easy, 2: Medium, 3: Hard)
  * @param boats
  * @param sentShots
  * @param receivedShots
  * @param score
  */
case class AIPlayer(
    override val name: String,
    val level: Int = 1,
    override val boats: List[Boat] = List(), 
    override val sentShots: List[Shot] = List(), 
    override val receivedShots: List[Shot] = List(),
    override val score: Int = 0
)  
extends Player(name, boats, sentShots, receivedShots){
    override def askForBoat(otherBoats: List[Boat], size: Int): Boat = {
        val r: Random = Game.random

        //Place boat randomly
        val headx: Int = r.nextInt(GameSettings.gridSize)+1
        val heady: Int = r.nextInt(GameSettings.gridSize)+1
        val direction: String = r.nextInt(4)+1 match {
            case 1 => "T"
            case 2 => "R"
            case 3 => "B"
            case 4 => "L"
        }
        val boat: Boat = Boat(size, headx, heady, direction)

        //Check id the boat is valid
        if (boat.isCrossingBoat(otherBoats) || boat.isOutGrid){
            askForBoat(otherBoats, size)
        }
        else boat
    }

    override def addSentShot(shot: Shot): Player = {
        new AIPlayer(name, level, boats, shot::sentShots, receivedShots, score)
    }

    override def addReceivedShot(shot: Shot): Player = {
        new AIPlayer(name, level, boats, sentShots, shot::receivedShots, score)
    }

    override def askForShot(): Shot = {
        
        val shot: Shot = level match {
            // Hard AI shot
            case 3 => AIPlayer.shotByFollowingBoat(sentShots, Shot.randomModuloShot)
            // Medium AI shot
            case 2 => AIPlayer.shotByFollowingBoat(sentShots, Shot.randomShot)
            // Easy AI shot
            case _ => {
                Shot.randomShot(sentShots)
            }
        }

        DisplayHelper.shotThere(shot, this)
        shot
    }

    override def addScore(): Player = {
        AIPlayer(name, level, boats, sentShots, receivedShots, score+1)
    }

}

object AIPlayer {
    def apply(name: String): AIPlayer = {
        new AIPlayer(name)
    }

    /**
      * Return a shot from an AI. The goal is to try to touch all the ship before searching another one with random shot.
      * @param sentShots List of the already sent shots in the game by the AI.
      * @param randomShot A function that make a random shot from a list of alredy sent shots.
      * @return The shot that result of the AI.
      */
    def shotByFollowingBoat(sentShots: List[Shot], randomShot: (List[Shot]) => Shot ): Shot = {

        // Find the last touchedshot
        val lastTouchedShotOption: Option[Shot] = Shot.lastTouchedShot(sentShots)

        lastTouchedShotOption match {
            // If there is no last touched shot (in time)
            case None =>{
                randomShot(sentShots)
            }
            case Some(lastTouchedShot) => {
                // If the last touched shot sank a boat (in time)
                if(lastTouchedShot.sankBoat) randomShot(sentShots)
                // Else if we find the line with the first (in time) touched shot of the last boat
                else{
                    val firstTouchedShotOption: Option[Shot] = Shot.firstTouchedShot(sentShots)
                    val firstTouchedShot = firstTouchedShotOption.get

                    if(firstTouchedShot.pos.equal(lastTouchedShot.pos)){
                        //If the first and last are the same then search around if possible else random
                        Shot.shotAround(firstTouchedShot, sentShots).getOrElse(randomShot(sentShots))
                    }
                    else{
                        //Else shot near on the same line
                        Shot.shotNear(firstTouchedShot, lastTouchedShot, sentShots).getOrElse(randomShot(sentShots))
                    }
                }
            }
        }
    }
}