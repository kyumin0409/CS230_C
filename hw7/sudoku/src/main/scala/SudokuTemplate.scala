import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  val peersTbl = Map((0.to(8)).flatMap{
  	r=>0.to(8).map{
  		c=>((r,c)->peerHelper(r,c))
  	}}:_*)

  def deleteNum(ls: List[Int], x:Int):List[Int] = ls match{
  		case Nil => Nil
  		case head :: tail =>{
  			if(head == x){
  				deleteNum(tail,x)
  			}
  			else{
  				head :: deleteNum(tail,x)
  			}
  		}
  }

  def newBoard(pos:(Int,Int),ls: List[(Int,Int)],x:Int,board:List[((Int,Int),List[Int])]) : List[((Int,Int),List[Int])] = board match{
  		case Nil => Nil
  		case head :: tail =>{
  			if(pos == head._1){
  				((pos),List[Int](x)) :: newBoard(pos,ls,x,tail)
  			}
  			else if (ls.contains(head._1)){
  				val a : (Int,Int) = head._1
  				val b : List[Int] = deleteNum(head._2,x)
  				((a),b) :: newBoard(pos,ls,x,tail)
  				//List[(Int,Int),List[Int]](((head._1).asInstanceOf[(Int,Int)],deleteNum(head._2,x).asInstanceOf[List[Int]])) :: newBoard(ls,x,tail)
  			}
  			else{
  				head :: newBoard(pos,ls,x,tail)
  			}
  		}
  }

  val parseDefault = Map((0.to(8)).flatMap{
  		r => 0.to(8).map{
  			c => ((r,c)->List(1,2,3,4,5,6,7,8,9))
  		}
  		}:_*)

  def finalBoard (s: String, ls: List[((Int,Int),List[Int])], index:Int) : List[((Int,Int),List[Int])] = {
  		if(s.length < 1){
  			ls
  		}else{
  			s.substring(0,1) match{
  				case "" => ls
  				case "." => finalBoard(s.substring(1),ls,index+1)
  				case a => {
  					val posRow = index.toInt/9
  					val posCol = index.toInt%9
  					val listPeers = peers(posRow,posCol)
  					val value = a.toInt
  					finalBoard(s.substring(1),newBoard((posRow,posCol),listPeers, value, ls),index+1)
  				}
  			}
  		}
  }

  def parse(str: String): Board = {
  	val b = finalBoard(str,parseDefault.toList,0).toMap
  	new Board(b)
  }

  

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    peersTbl((row,col))
  }

  def peerHelper(row: Int, col: Int) : List[(Int,Int)] = {
  	val boxRow : Int = (row/3)*3
  	val boxCol : Int = (col/3)*3
  	val boxPeers = (boxRow.to(boxRow+2)).flatMap(r=>boxCol.to(boxCol+2).map(c=>(r,c)))
  	val rowPeers = 0.to(8).map(r=>(r,col))
  	val colPeers = 0.to(8).map(c=>(row,c))
  	(rowPeers ++ colPeers ++ boxPeers).filterNot{
  		case(r,c) => (r,c) == (row,col)
  	}.toList.distinct
  }
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    val ls = availableValuesAt(row,col)
    if(ls.size>1 || ls.size==0){
    	None
    }
    else{
    	Some(ls.head)
    }
  }

  def isSolved(): Boolean = {
    isSolvedHelper(0,0)
  }

  def isSolvedHelper(row:Int, col:Int) : Boolean = {
  	val ls = valueAt(row,col)
  	if(row==8 && col==8){
  		if(ls == None) false
  		else true
  	}
  	else if(row == 8){
  		if(ls == None) false
  		else{
  			isSolvedHelper(0,col+1)
  		}
  	}
  	else {
  		if(ls == None) false
  		else{
  			isSolvedHelper(row+1,col)
  		}
  	}
  }

  def isUnsolvable(): Boolean = {
    if(isUnsolvableHelper(0,0) == true && isSolved == false) true
    else false
  }

  def isUnsolvableHelper(row:Int, col:Int) : Boolean = {
  	val ls = availableValuesAt(row,col)
  	if(row == 8 && col == 8){
  		if(ls.size == 0) true
  		else false
  	}
  	else if(row == 8){
  		if(ls.size == 0) true
  		else{
  			isUnsolvableHelper(0,col+1)
  		}
  	}
  	else{
  		if(ls.size == 0) true
  		else{
  			isUnsolvableHelper(row+1,col)
  		}
  	}
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    //new Board(/*recursiveCheck(*/newPlaceBoard((row,col),Solution.peers(row,col),value,available.toList)/*)*/.toMap)
  	val peersList = Solution.peers(row,col)
  	val newB = newPlaceBoard((row,col), peersList, value, available.toList)
  	val valuesAtPeers = valAtPeers(peersList, new Board(newB.toMap))
  	placeHelper(row, col, value, new Board(newB.toMap), valuesAtPeers)
  }


  def newPlaceBoard(pos:(Int,Int),ls: List[(Int,Int)],x:Int,board:List[((Int,Int),List[Int])]) : List[((Int,Int),List[Int])] = board match{
  		case Nil => Nil
  		case head :: tail => {
  			if(pos == head._1){
  				((pos),List[Int](x)) :: newPlaceBoard(pos,ls,x,tail)
  			}
  			else if(ls.contains(head._1)){
  				((head._1),Solution.deleteNum(head._2,x)) :: newPlaceBoard(pos,ls,x,tail)
  			}
  			else{
  				head :: newPlaceBoard(pos,ls,x,tail)
  			}
  		}
  		
  }

  def valAtPeers (peers: List[(Int,Int)], board: Board) : List[((Int,Int), List[Int])] = peers match{
  	case Nil => Nil
  	case head :: tail => ((head), board.availableValuesAt(head._1, head._2)) :: valAtPeers(tail,board)
  }

  def placeHelper(row: Int, col: Int, value: Int, board: Board, peersVal: List[((Int,Int), List[Int])]) : Board = peersVal match{
  	case Nil => board
  	case head :: tail => {
  		if(board.isSolved || board.isUnsolvable) board
  		else if(head._2.size == 1){
  			val nRow = head._1._1
  			val nCol = head._1._2
  			val nVal = head._2.head
  			val nPeers = Solution.peers(nRow,nCol)
  			val nBoard = newPlaceBoard((nRow,nCol), nPeers, nVal, board.available.toList)
  			val nPVal = valAtPeers(nPeers,new Board(nBoard.toMap)) filterNot{
  				case (pos, coor) => coor.size == 1
  			}
  			placeHelper(nRow,nCol,nVal,new Board(nBoard.toMap), nPVal)
  		}
  		else{
  			placeHelper(row, col, value, board, tail)
  		}
  	}
  }

  

  	def findOneValueCells(board: List[((Int,Int),List[Int])]) : List[((Int,Int),List[Int])] = board match{
  		case Nil => Nil
  		case head :: tail =>{
  			val values = availableValuesAt(head._1._1,head._1._2)
  					val size = values.size
  					if(size == 1){
  						head :: findOneValueCells(tail)
  					}
  					else{
  						findOneValueCells(tail)
  					}
  		}
  	}

  	
 

  	
  

  

  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if (isUnsolvable()) {
      List()
    }
    else{
    	val mTOneLst = moreThanOne(available.toList)
    	val lst = placeList(mTOneLst, this)


    	def s (board1: Board, board2:Board) : Boolean = {
    		val b1 = findOneValueCells(board1.available.toList)
    		val b2 = findOneValueCells(board2.available.toList)
    		if(b1.size>b2.size) true
    		else false
    	}

    	def insertOrdered(n: Board, lst: List[Board]): List[Board] = lst match{
    		case Nil => n :: Nil
    		case head :: tail => {
    			if(s(n,head) == true){
    				head :: insertOrdered(n, tail)
    			}else{
    				n::head::tail
    			}
    		}
    	}

    	def sort(lst: List[Board]) : List[Board] = lst match{
    		case Nil => Nil
    		case head :: Nil => head :: Nil
    		case head :: tail => insertOrdered(head, sort(tail))
    	}

    	sort(lst)

    	/*val board = available.toList
    	val moreThanOneLst = moreThanOne(board)
    	val lstBoard = nextStatesHelper(moreThanOneLst, available.toList)

    	

    	def sort (lst: List[Board]) : List[Board] = lst match{
    		case Nil => Nil
    		case h1::h2::tail => {
    			if(s(h1,h2) == true){
    				sort(h1 :: sort(h2::tail))
    			}
    			else{
    				sort(h2 :: sort(h1::tail))
    			}
    		}
    		case h :: Nil => h :: Nil
    	}

    	sort(lstBoard)*/
    }

    
  }

  def placeList (moreTO : List[((Int,Int), List[Int])], board: Board) : List[Board] = moreTO match{
  	case Nil => Nil
  	case head :: tail =>{
  		val r = head._1._1
  		val c = head._1._2
  		actualPlace(r,c,head._2,board) ++ placeList(tail, board)
  	}
  }

  def actualPlace (row: Int, col: Int, lst: List[Int], board: Board) : List[Board] = lst match{
  	case Nil => Nil
  	case head :: tail => board.place(row, col, head) :: actualPlace(row,col,tail,board)
  }
  
  def moreThanOne (lst : List[((Int,Int),List[Int])]) : List[((Int,Int),List[Int])] = lst match{
  	case Nil => Nil
  	case head :: tail =>{
  		if(head._2.size >1) head :: moreThanOne(tail)
  		else moreThanOne(tail)
  	}
  }

  



  def solve(): Option[Board] = {
    solveHelper(this)
  }

  def solveHelper(board : Board) : Option[Board] = {
  	if(board.isSolved) Some(board)
  	else{
  		val nextList = board.nextStates
  		nsIter(nextList)
  	}
  }

  def nsIter(lst : List[Board]) : Option[Board] = lst match{
  	case Nil => None
  	case head :: tail => {
  		val solution = solveHelper(head)
  		if(solution == None) nsIter(tail)
  		else solution
  	}
  }
}