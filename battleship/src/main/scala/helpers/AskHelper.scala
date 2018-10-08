package helpers

import players._
import game.GameSettings
import boats.Boat
import scala.util.matching.Regex
import grid._

object AskHelper {
    
    def boatHeadX(size: Int): Int = {
        println("Ship of size : " + size + ". \u001b[36mChoose a x location\u001b[0m")
        checkIntWithSize(getUserInput, boatHeadX, size)
    }

    def boatHeadY(size: Int): Int = {
        println("Ship of size : " + size + ". \u001b[35mChoose a y location\u001b[0m")
        checkIntWithSize(getUserInput, boatHeadY, size)
    }

    def boatDirection(size: Int): String = {
        println("Ship of size : " + size + ". Choose a direction : (T)op, (L)eft, (B)ottom, (R)ight")
        val userInput: String = getUserInput.toUpperCase
        userInput match {
            case "T"|"L"|"B"|"R" => userInput
            case _ => {
                println("\u001b[33mYou have to enter 'T', 'L', 'B' or 'R'. Retry.\u001b[0m")
                boatDirection(size)
            }
        }
    }

    def shotx(): Int = {
        println("\u001b[36mX position of the shot ?\u001b[0m")
        checkInt(getUserInput, shotx)
    }

    def shoty(): Int = {
        println("\u001b[35mY position of the shot ?\u001b[0m")
        checkInt(getUserInput, shoty)
    }

    def continue(): Unit = {
        println("Continue (Enter) ?")
        getUserInput
    }

    def continuOrQuit(nextPlayer: Player): String = {
        println("Continu with " + nextPlayer.name + " (Enter) ? Or (Q)uit ?")
        getUserInput.toUpperCase()
    }

    def nextPlayer(nextPlayer: Player): Unit = {
        println("Continue with " + nextPlayer.name + " ?")
        getUserInput.toUpperCase()
    }

    def userOrAI(name: String): Player = {
        println("Would you like to play with an (U)ser or with an AI with level (1), (2) or (3) ?")
        val userInput: String = getUserInput.toUpperCase
        userInput match {
            case "U" => UserPlayer(name)
            case "1" => AIPlayer(name)
            case "2" => AIPlayer(name, 2)
            case "3" => AIPlayer(name, 3)
            case _ => {
                println("\u001b[33mYou have to enter 'U', '1', '2' or '3'. Retry.\u001b[0m")
                userOrAI(name)
            }
        }
    }

    def checkIntWithSize(userInput: String, f: (Int) => Int , size: Int): Int = {
        val Pattern = "([0-9]+)".r
        userInput match {
            case Pattern(c) => userInput.toInt
            case _ => {
                println("\u001b[33mYou have to enter an integer. Retry.\u001b[0m")
                f(size)
            }
        }
    }

    def checkInt(userInput: String, f: () => Int): Int = {
        if(userInput.matches("[0-9]+")) 
            userInput.toInt 
        else{
            println("\u001b[33mYou have to enter an integer. Retry.\u001b[0m")
            f()
        }
    }

    def contestOrGame(): String = {
        println("What would you like to do ?")
        println("1) Game between user and/or AI")
        println("2) Simple Contest of two AIs")
        println("3) Big Contest (Contest between all AIs)")
        println("4) Quit")

        val userInput: String = getUserInput.toUpperCase
        userInput match {
            case "1"|"2"|"3"|"4" => userInput
            case _ => {
                println("\u001b[33mYou have to enter '1', '2' or '3'. Retry.\u001b[0m")
                contestOrGame()
            }
        }
    }

    def whichAI(name: String): AIPlayer = {
        println("Which AI level for "+ name +" ? (1), (2) or (3) ?")
        val userInput: String = getUserInput
        userInput match {
            case "1" => AIPlayer(name + " (level " + userInput + ")", 1)
            case "2" => AIPlayer(name + " (level " + userInput + ")", 2)
            case "3" => AIPlayer(name + " (level " + userInput + ")", 3)
            case _ => {
                println("\u001b[33mYou have to enter '1', '2' or '3'. Retry.\u001b[0m")
                whichAI(name)
            }
        }
    }

    def returnToMenu(): Boolean = {
        println("Return to menu (Enter) ? Or (Q)uit ?")
        val userInput: String = getUserInput.toUpperCase()
        userInput != "Q"
    }

    def getUserInput(): String = readLine.trim
}