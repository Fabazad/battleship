package helpers

import players.Player
import game.GameSettings
import boats.Boat
import scala.util.matching.Regex

object AskHelper {
    
    def boatHeadX(size: Int): Int = {
        println("Boat of size : " + size + ". Choose a x location")
        checkIntWithSize(getUserInput, boatHeadX, size)
    }

    def boatHeadY(size: Int): Int = {
        println("Boat of size : " + size + ". Choose a y location")
        checkIntWithSize(getUserInput, boatHeadY, size)
    }

    def boatDirection(size: Int): String = {
        println("Boat of size : " + size + ". Choose a direction : (T)op, (L)eft, (B)ottom, (R)ight")
        val userInput: String = getUserInput.toUpperCase
        if(userInput.matches("T|L|B|R")) 
            userInput
        else{
            println("You have to put 'T', 'L', 'B' or 'R'.")
            boatDirection(size)
        }
    }

    def shotx(): Int = {
        println("X position of the shot ?")
        checkInt(getUserInput, shotx)
    }

    def shoty(): Int = {
        println("Y position of the shot ?")
        checkInt(getUserInput, shoty)
    }

    def continuOrQuit(nextPlayer: Player): String = {
        println("Continu with " + nextPlayer.name + " ? Or (Q)uit ?")
        getUserInput.toUpperCase()
    }

    def nextPlayer(nextPlayer: Player): Unit = {
        println("Continu with " + nextPlayer.name + " ?")
        getUserInput.toUpperCase()
    }

    def checkIntWithSize(userInput: String, f: (Int) => Int , size: Int): Int = {
        if(userInput.matches("[0-9]+")) 
            userInput.toInt 
        else{
            println("You have to put an integer.")
            f(size)
        }
    }

    def checkInt(userInput: String, f: () => Int): Int = {
        if(userInput.matches("[0-9]+")) 
            userInput.toInt 
        else{
            println("You have to put an integer.")
            f()
        }
    }

    def getUserInput(): String = readLine.trim
}