package grid

case class Pos(val x: Int, val y: Int){
    def equal(pos: Pos): Boolean = {
        x == pos.x && y == pos.y
    }
}
