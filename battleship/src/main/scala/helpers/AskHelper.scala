package helpers

import players.Player
import game.GameSettings
import boats.Boat

object AskHelper {
    
    def boatHeadX(size: Int): Int = {
        println("Boat of size : " + size + ". Choose a x location")
        getUserInput.toInt
    }

    def boatHeadY(size: Int): Int = {
        println("Boat of size : " + size + ". Choose a y location")
        getUserInput.toInt
    }

    def boatDirection(size: Int): String = {
        println("Boat of size : " + size + ". Choose a direction : (T)op, (L)eft, (B)ottom, (R)ight")
        getUserInput
    }

    def shotx(): Int = {
        println("X position of the shot ?")
        getUserInput.toInt
    }

    def shoty(): Int = {
        println("Y position of the shot ?")
        getUserInput.toInt
    }

    def getUserInput(): String = readLine.trim
}