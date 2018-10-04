package players

case class Shot(
    val x: Int, 
    val y: Int, 
    val touched: Boolean = false, 
    val boatSize: Int = 0, 
    val sankBoat: Boolean = false
){
    def setTouched(newTouched: Boolean): Shot = {
        Shot(x, y, newTouched, boatSize, sankBoat)
    }

    def setBoatSize(newBoatSize: Int): Shot = {
        Shot(x, y, touched, newBoatSize, sankBoat)
    }

    def setSankBoat(newSankBoat: Boolean): Shot = {
        Shot(x, y, touched, boatSize, newSankBoat)
    }
}

object Shot{

}