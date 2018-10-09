package helpers

import players._
import scala.util.matching.Regex
import board._
import scala.annotation.tailrec

/**
  * The Helper object that contains all the methods for user interaction with terminal.
  */
object AskHelper {

    /**
      * Ask for the x position of the head of a new boat.
      * @param size The size of the ship.
      * @return The x position entered by the user.
      */
    def boatHeadX(size: Int): Int = {
        println("Ship of size : " + size + ". \u001b[36mChoose a x location\u001b[0m")
        checkIntWithSize(getUserInput, boatHeadX, size)
    }

    /**
      * Ask for the y position of the head of a new boat.
      * @param size The size of the ship.
      * @return The y position entered by the user.
      */
    def boatHeadY(size: Int): Int = {
        println("Ship of size : " + size + ". \u001b[35mChoose a y location\u001b[0m")
        checkIntWithSize(getUserInput, boatHeadY, size)
    }

    /**
      * Ask the direction of the new ship to the user.
      * @param size The size of the ship.
      * @return A string that represent the direction of the ship. 'T' for Top, 'R' for Right, 'B' for Bottom and 'L' for Left.
      */
    @tailrec
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

    /**
      * Ask for the x position of a new shot.
      * @return The x position entered by the user.
      */
    def shotx(): Int = {
        println("\u001b[36mX position of the shot ?\u001b[0m")
        checkInt(getUserInput, shotx)
    }

    /**
      * Ask for the y position of a new shot.
      * @return The y position entered by the user.
      */
    def shoty(): Int = {
        println("\u001b[35mY position of the shot ?\u001b[0m")
        checkInt(getUserInput, shoty)
    }

    /**
      * Ask the user if he wants to continue, he just has to click on Enter.
      */
    def continue(): Unit = {
        println("Continue (Enter) ?")
        getUserInput
    }

    /**
      * Ask the User if he wants to continue or to quit the app before change turn.
      * @param nextPlayer The player whose is next turn
      * @return True if he wants to continue.
      */
    def continuOrQuit(nextPlayer: Player): Boolean = {
        println("Continu with " + nextPlayer.name + " (Enter) ? Or (Q)uit ?")
        getUserInput.toUpperCase() != "Q"
    }

    /**
      * Ask the User if he wants to play with an user or an AI, and if Ai then wich level.
      * @param name The name of the player to be created.
      * @return The new player agains who the user wants to play.
      */
    @tailrec
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

    /**
      * Check if the answer is an Integer else re-ask the user.
      * @param userInput The answer of the user.
      * @param f The function that which need to check the answer.
      * @param size Size of the ship.
      * @return The userInput if the userInput is an Integer else remake the asking function.
      */
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

    /**
      * Check if the answer is an Integer else re-ask the user.
      * @param userInput The answer of the user.
      * @param f The function which we need to check the answer.
      * @return The userInput if the userInput is an Integer else remake the asking function.
      */
    def checkInt(userInput: String, f: () => Int): Int = {
        if(userInput.matches("[0-9]+")) 
            userInput.toInt 
        else{
            println("\u001b[33mYou have to enter an integer. Retry.\u001b[0m")
            f()
        }
    }

    /**
      * Ask the user if wants to make a game, a contest or a big contest. Or also if he wants to quit the app.
      * @return Return "1", "2", "3" or "4" depending of the user response.
      */
    @tailrec
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

    /**
      * Which of the AI the user wants to play with.
      * @param name name of the future AI player.
      * @return The AI player with a level and a name.
      */
    @tailrec
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

    /**
      * Ask the user if he wants to return to the menu or quit the game.
      * @return True if the user wants to continue else false.
      */
    def returnToMenu(): Boolean = {
        println("Return to menu (Enter) ? Or (Q)uit ?")
        val userInput: String = getUserInput.toUpperCase()
        userInput != "Q"
    }

    /**
      * Ask to continue with the next player.
      * @param nextPlayer The next player.
      */
    def nextPlayer(nextPlayer: Player): Unit = {
        println("Continue with " + nextPlayer.name + " ?")
        getUserInput.toUpperCase()
    }

    /**
      * Get the user input, the one he put in the console.
      * @return The value of the user input.
      */
    def getUserInput(): String = readLine.trim
}