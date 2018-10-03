package players

import boats.Boat
import helpers.AskHelper
import helpers.DisplayHelper
import game.GameSettings

abstract class Player(val name: String, val boats: List[Boat]) {
    def askForBoats(gs: GameSettings): Player

    def askForBoat(otherBoats: List[Boat], size: Int, gs: GameSettings): Boat

    def shot(target: Player, gs: GameSettings): Shot
}