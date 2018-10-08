package game

import players.Player
import scala.util.Random
import grid._

/**
  * The game class which is use as a state.
  * @param player1 First player of the game.
  * @param player2 Second player of the game.
  */
case class Game(player1: Player, player2: Player){

}

object Game {
    val random: Random = new Random()
}