package players

import boats.Boat

class UserPlayer(override val boats: List[Boat] = List()) extends Player(boats){
    
}