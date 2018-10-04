package game

import players.Player
import scala.util.Random

case class Game(player1: Player, player2: Player){

}

object Game {
    val random: Random = new Random()
}