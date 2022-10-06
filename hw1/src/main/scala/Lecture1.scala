object Lecture1 {
	
	val oddNumbers = 1 :: 3 :: 5 :: Nil

	def sumDouble (list : List[Int]) : Int = list match{
		case Nil =>0
		case head :: tail => 2*head + sumDouble(tail)
	}

	def removeZeroes (list: List[Int]) : List[Int] = list match {
		case Nil => Nil
		case 0 :: tail => removeZeroes(tail)
		case n :: tail => n :: removeZeroes(tail)
	}

	def countEvens (list: List[Int]) : Int = list match{
		case Nil => 0
		case head :: tail =>{
			if(head%2==0)
				1+countEvens(tail)
			else
				countEvens(tail)
		}
		
	}

	def removeAlternating (list: List[String]) : List[String] ={
		removeAlternatingHelper(list,0)
	}

	def removeAlternatingHelper (list: List[String], num: Int) : List[String] = list match{
		case Nil => Nil
		case head :: tail =>{
			if(num%2==0)
				head :: removeAlternatingHelper(tail, num+1)
			else
				removeAlternatingHelper(tail, num+1)
		}
	}

	def isAscending (list: List[Int]) : Boolean = list match{
		case Nil => true
		case head :: Nil =>{
			true
		}
		case head :: head2 ::tail =>{
			if(tail == Nil)
				true
			if(head <= head2){
				isAscending(head2 :: tail)
			}
			else
				false
		}
		
		
	}

	def addSub (list: List[Int]) : Int ={
		addSubHelper(list, 0)
	}

	def addSubHelper (list: List[Int], num: Int) : Int = list match{
		case Nil => 0
		case head :: tail =>{
			if(num%2==0)
				addSubHelper(tail, num+1)+head
			else
				addSubHelper(tail, num+1)-head
		}
	}	

	def alternate (list1: List[Int], list2: List[Int]) : List[Int] = (list1, list2) match{
		case (Nil, Nil) => Nil
		case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: alternate(tail1, tail2)
		case (_, _) => Nil
	}

	def fromTo (num1: Int, num2: Int) : List[Int] ={
		if(num1>=num2) Nil
		else num1 :: fromTo(num1+1, num2)
	}

	def insertOrdered (n: Int, list: List[Int]) : List[Int] = list match {
		case Nil => n :: Nil
		case head :: tail =>{
			if(isAscending(head :: tail) == false)
				sys.error("list is not in order")
			if(n<=head)
				n :: head :: tail
			else
				head :: insertOrdered(n, tail)
		}
	}
	
	def sort (list: List[Int]) : List[Int] = list match{
		case Nil => Nil
		case head :: Nil => head :: Nil
		case head :: tail =>
			insertOrdered(head, sort(tail))
	}

	
	
}