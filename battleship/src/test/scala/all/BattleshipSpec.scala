package all

import org.scalatest._
import boats._
import game._
import grid._
import helpers._
import players._

class BattlefieldSpec extends FlatSpec with Matchers {
  "The addSentShot method" should "add a sent shot" in {
    val shot: Shot = Shot(Pos(1,2))
    val player: Player = UserPlayer("Test")
    player.sentShots.length should be (0)
    player.addSentShot(shot).sentShots.length should be (1)
  }

  "The addReceivedShot method" should "add a received shot" in {
    val shot: Shot = Shot(Pos(1,2))
    val player: Player = UserPlayer("Test")
    player.receivedShots.length should be (0)
    player.addReceivedShot(shot).receivedShots.length should be (1)
  }

  "The addScore method" should "add one to the user score" in {
    val player: Player = UserPlayer("Test")
    player.score should be (0)
    player.addScore.addScore.score should be (2)
  }

  "The apply of UserPlayer with only name" should "create an empty player" in {
    val player: Player = UserPlayer("Test")
    player.boats.length should be (0)
    player.sentShots.length should be (0)
    player.receivedShots.length should be (0)
  }

  "The apply of AIPlayer with only name" should "create an empty player" in {
    val player: Player = AIPlayer("Test")
    player.boats.length should be (0) 
    player.sentShots.length should be (0) 
    player.receivedShots.length should be (0)
  }

  "The askForBoats of an AIPlayer" should "create a Player with a right number of boat with right sizes" in {
    val player: Player = AIPlayer("Test")
    player.boats.length should be (0)
    player.askForBoats.boats.length should be (GameSettings.boats.size)

  }

  "The askForBoats of an AIPlayer" should "create a Player with valid boats" in {
    val player: Player = AIPlayer("Test").askForBoats
    player.boats.filter((b) => b.isOutGrid).length should be (0)
    val allCells: List[Cell] = player.boats.flatMap((b) => b.cells)
    allCells.filter(c1 => {
      allCells.filter(c2 => c1.pos.equal(c2.pos)).length > 1
    }).length should be (0)
  }

  "Two user with the same name" should "be equals, else not" in {
    val player1: Player = UserPlayer("test")
    val player2: Player = AIPlayer("test")
    val player3: Player = UserPlayer("other")

    player1.equal(player2) should be (true)
    player1.equal(player3) should be (false)
  }

  "The shotByFollowingBoat method" should "shot second around the first touched shot" in {
    val sentShots: List[Shot] = List(Shot(Pos(5,5),true))
    val shot: Shot = AIPlayer.shotByFollowingBoat(sentShots, Shot.randomShot)
    Some(shot.pos.x) should contain oneOf (4,5,6)
    Some(shot.pos.y) should contain oneOf (4,5,6)
  }

  "The isAlreadyShot method for shot" should "works" in {
    val shot: Shot = Shot(Pos(5,5))
    val shots1: List[Shot] = List(Shot(Pos(4,4)), Shot(Pos(8,6)))
    shot.isAlreadyShot(shots1) should be (false)

    val shots2: List[Shot] = Shot(Pos(5,5))::shots1
    shot.isAlreadyShot(shots2) should be (true)
  }

  "The shotNear mmethod" should "create a shot in the same line of two touched shot" in {
    val shot1: Shot = Shot(Pos(4,5),true)
    val shot2: Shot = Shot(Pos(4,6),true)
    val shot3: Shot = Shot(Pos(4,7),true)
    val shots: List[Shot] = List(shot1, shot2, shot3)
    val lastShot: Option[Shot] = Shot.shotNear(shot1, shot3, shots)
    lastShot.get.pos.x should be (4)
    Some(lastShot.get.pos.y) should contain oneOf (4,8)
  }

  "The randomModuloShot function" should "find a shot where x+y is even" in {
    val shot: Shot = Shot.randomModuloShot(List())
    (shot.pos.x + shot.pos.y) % 2 should be (0)
  }

  "The allPos function" should "find the right number of pos" in {
    Pos.allPos.length should be (GameSettings.gridSize*GameSettings.gridSize)
  }
}
