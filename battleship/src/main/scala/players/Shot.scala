package players

case class Shot(val x: Int, val y: Int, val touched: Boolean){
    def setTouched(touched: Boolean): Shot = {
        new Shot(x, y, touched)
    }
}

object Shot{

}