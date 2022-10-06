object Homework2 {
	
	def map2[A,B,C](f: (A,B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1, lst2) match{
		case (Nil,Nil) => Nil
		case (head1 :: tail1, head2 :: tail2) => {
			if(listLength(lst1, lst2) == false)
				sys.error("lst1 and lst2 do not have the same length")
			f(head1, head2) :: map2(f, tail1, tail2) 
		}
		case (_,_) => Nil
	}

	def zip[A,B](lst1: List[A], lst2: List[B]): List[(A,B)] = (lst1, lst2) match{
		case (Nil,Nil) =>Nil
		case (head1 :: tail1, head2 :: tail2) => {
			if(listLength(lst1, lst2) == false)
				sys.error("lst1 and lst2 do not have the same length")
			(head1,head2) :: zip(tail1, tail2)
		}
		case (_,_) => Nil
	}

	def flatten[A](lst: List[List[A]]): List[A] = lst match{
		case Nil => Nil
		case head :: tail => append(head, flatten(tail))
	}

	def append[A](lst1: List[A], lst2: List[A]) : List[A] = (lst1, lst2) match{
		case (Nil,Nil) => Nil
		case (Nil, head :: tail) => head :: append(Nil, tail)
		case (head :: tail, _) => head :: append(tail, lst2)
	}

	def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match{
		case Nil => Nil
		case head :: tail => append(flatten(head),flatten3(tail))
	}

	def buildList[A](length: Int, f: Int => A): List[A] = {
		buildListHelper(length, f, 0)
	}

	def buildListHelper[A](length: Int, f: Int => A, index: Int): List[A] = {
		if(index >= length)
			Nil
		else
			f(index) :: buildListHelper(length, f, index+1)
	}

	def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match{
		case Nil => Nil
		case head :: tail => append(f(head), mapList(tail, f))
	}

	def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) ={
		(partitionTrue(f, lst), partitionFalse(f, lst))
	}

	def partitionTrue[A](f: A => Boolean, lst: List[A]): List[A] = lst match{
		case Nil => Nil
		case head :: tail =>{
			if(f(head) == true)
				head :: partitionTrue(f, tail)
			else
				partitionTrue(f, tail)
		}
	}

	def partitionFalse[A](f: A => Boolean, lst: List[A]): List[A] = lst match{
		case Nil => Nil
		case head :: tail =>{
			if(f(head) == false)
				head :: partitionFalse(f, tail)
			else
				partitionFalse(f, tail)
		}
	}

	def listLength[A, B](lst1: List[A], lst2: List[B]): Boolean = (lst1, lst2) match {
		case (Nil, Nil) => true
		case (head :: tail, Nil) => false
		case (Nil, head :: tail) => false
		case (head1 :: tail1, head2 :: tail2) => listLength(tail1, tail2)
		case (_, _) => false
	}
}