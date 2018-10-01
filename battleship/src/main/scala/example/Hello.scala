package example
import helpers.DisplayHelper 

object Hello extends Greeting with App {
  DisplayHelper.rules()
}

trait Greeting {
  lazy val greeting: String = "hello"
  def test(): Unit = { 
    println("test")
    println(greeting)
  }
}
