package helpers

/**
  * Settings of the game.
  */
object GameSettings {
    val gridSize: Int = 10
    val boats: List[Int] = List(5,4,3,3,2)
    val touchedDisplay: String = "\u001b[32mo\u001b[0m"
    val untouchedDisplay: String = "\u001b[31mo\u001b[0m"
    val contestGames: Int = 100
}