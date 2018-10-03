package players

import boats.Boat
import helpers.AskHelper
import helpers.DisplayHelper

abstract class Player(val name: String, val boats: List[Boat]) {
    def askForBoats(): Player

    def askForBoat(otherBoats: List[Boat], size: Int): Boat

    def shot(target: Player): Shot
}