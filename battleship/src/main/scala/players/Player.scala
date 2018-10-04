package players

import boats._
import helpers.AskHelper
import helpers.DisplayHelper

abstract class Player(val name: String, val boats: List[Boat], val sentShots: List[Shot], val receivedShots: List[Shot]) {
    def askForBoats(): Player

    def askForBoat(otherBoats: List[Boat], size: Int): Boat

    def shot(target: Player): Shot

    def addSentShot(shot: Shot): Player

    def addReceivedShot(shot: Shot): Player
    
    def lose(): Boolean = {
        val allCells: List[Cell] = boats.flatMap((b) => b.cells)
        allCells.filter((c) => !c.isTouched()).length == 0
    }
}