package game

case class GameSettings()

object GameSettings {
    val gridSize: Int = 10
    val boats: List[Int] = List(5, 4)
}