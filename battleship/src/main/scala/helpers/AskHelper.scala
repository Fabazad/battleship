package helpers

import players._
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
        userInput match {
            case "T"|"L"|"B"|"R" => userInput
            case _ => {
                println("You have to put 'T', 'L', 'B' or 'R'. Retry.")
                boatDirection(size)
            }
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

    def userOrAI(name: String): Player = {
        println("Would you like to play with an (U)ser or with an (A)I ?")
        val userInput: String = getUserInput.toUpperCase
        userInput match {
            case "U" => UserPlayer(name)
            case "A" => AIPlayer(name)
            case _ => {
                println("You have to put 'U' or 'A'. Retry.")
                userOrAI(name)
            }
        }
    }

    def checkIntWithSize(userInput: String, f: (Int) => Int , size: Int): Int = {
        val Pattern = "([0-9]+)".r
        userInput match {
            case Pattern(c) => userInput.toInt
            case _ => {
                println("You have to put an integer. Retry.")
                f(size)
            }
        }
    }

    def checkInt(userInput: String, f: () => Int): Int = {
        if(userInput.matches("[0-9]+")) 
            userInput.toInt 
        else{
            println("You have to put an integer. Retry.")
            f()
        }
    }

    def getUserInput(): String = readLine.trim
}