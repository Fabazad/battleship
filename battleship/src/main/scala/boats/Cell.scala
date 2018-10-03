package boats

case class Cell(val x: Int, val y: Int, val state: String) {

    def changeState(newState: String): Cell = {
        Cell(x, y, newState)
    }
    
}