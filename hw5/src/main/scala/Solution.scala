class Game(m: Matrix[Option[Player]], turn: Player) extends GameLike[Game] {

  //orginal	
  def isFinished(): Boolean = m.toMap.values.toList.length == m.dim * m.dim

  /* Assume that isFinished is true *///original
  def getWinner(): Option[Player] = {
  	if(checkRowCol(m.rows(),X) == true) Some(X)
  	else if(checkRowCol(m.cols(),X) == true) Some(X)
  	else if(containsWinner(m.mainDiagonal(),X) == true) Some(X)
  	else if(containsWinner(m.antiDiagonal(),X) == true) Some(X)
  	else if(checkRowCol(m.rows(),O) == true) Some(O)
  	else if(checkRowCol(m.cols(),O) == true) Some(O)
  	else if(containsWinner(m.mainDiagonal(),O) == true) Some(O)
  	else if(containsWinner(m.antiDiagonal(),O) == true) Some(O)
  	else None
  }

  //helper
  def containsWinner(lst: List[Option[Player]], player: Player) : Boolean = lst match{
  	case Nil => true
  	case head :: tail =>{
  		if(head == Some(player))
  			containsWinner(tail,player)
  		else
  			false
  	}
  }

  //helper
  def checkRowCol(lst: List[List[Option[Player]]], player: Player) : Boolean = lst match{
  	case Nil => false
  	case head :: tail =>{
  		if(containsWinner(head,player) == true)
  			true
  		else
  			checkRowCol(tail,player)
  	}
  }

  //original
  def nextBoards(): List[Game] = {
  	
  	val p ={
  		if(turn == X)
  			O
  		else
  			X
  	}
  	
  	nextBoardsHelper(0,0,p)
  }

  def nextBoardsHelper(r:Int, c:Int, nextP: Player): List[Game] ={
  	if(r>=m.dim){
  		Nil
  	}
  	else if(c>=m.dim){
  		nextBoardsHelper(r+1,0,nextP)
  	}
  	else{
  		if(m.get(r,c) == None)
  			(new Game(m.set(r,c,Some(turn)),nextP)) :: nextBoardsHelper(r,c+1,nextP)
  		else
  			nextBoardsHelper(r,c+1,nextP)
  	}
  }



  //helper
  def mat():Matrix[Option[Player]] = m
  def turn():Player = turn
}

object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike

  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
  	new Game((Matrix.fromMap(dim,None,board.mapValues(p=>Some(p)))),turn)
  }

  def minimax(board: Game): Option[Player] = {
  	if(board.turn() == X){
  		if(board.isFinished() == true){
  			if(board.getWinner() == Some(X))
  				Some(X)
  			else if(board.getWinner() == Some(O))
  				Some(O)
  			else 
  				None
  		}
  		else{
  			board.nextBoards.map(minimax).head
  		}
  	}
  	else{
  		if(board.isFinished() == true){
  			if(board.getWinner() == Some(O))
  				Some(O)
  			else if(board.getWinner() == Some(X))
  				Some(X)
  			else 
  				None
  		}
  		else{
  			board.nextBoards.map(minimax).head
  		}
  	}
  	/*if(board.turn() == X){
  		if(board.isFinished()){
  			if(board.getWinner() == Some(X))
  				Some(X)
  			else
  				None
  		}
  		else{
  			minimaxHelper(board.nextBoards, O)
  			/*val iter = board.nextBoards.toIterator
  			while(iter.hasNext){
  				minimax(iter.next)
  			}
  			Some(O)*/
  		}
  	}
  	else{
  		if(board.isFinished()){
  			if(board.getWinner() == Some(O))
  				Some(O)
  			else
  				None
  		}
  		else{
  			minimaxHelper(board.nextBoards, X)
  			/*val iter = board.nextBoards.toIterator
  			while(iter.hasNext){
  				minimax(iter.next)
  			}
  			Some(X)*/
  		}

  	}*/
  }

  /*def minimaxHelper(lst: List[Game], p: Player):Option[Player] = lst match{
  	case Nil => {
  		val p2 = {
  			if(p == X)
  				O
  			else
  				X
  		}
  		Some(p2)
  	}
  	case head :: tail => {
  		minimaxHelper(tail, p)
  		minimax(head)
  	}
  }*/



}
