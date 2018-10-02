package game

case class GameSettings(gridSize: Int, boats: List[Int])

object GameSettings {
    val gridSize: Int = 10
    val boats: List[Int] = List(5)
    def apply(): GameSettings = {
        new GameSettings(gridSize, boats)
    }
}