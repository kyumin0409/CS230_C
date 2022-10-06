object FunctionalDataStructures {

  //
  // Part 1. Persistent Queues
  //

  def enqueue[A](elt: A, q: Queue[A]): Queue[A] = {
  	val lst1 = q.front
  	val lst2 = elt :: q.back
  	new Queue[A](lst1, lst2)
  }

  def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = {
  	val lst1 = q.front
  	val lst2 = q.back

  	if(lst1.length == 0)
  		None
  	else{
  		val elem = lst1.head
  		val lst3 = lst1.tail
  		val queue = new Queue[A](lst3,lst2)
  		Some((elem,queue))
  	}
  }

  //
  // Part 2. Join Lists
  //

  def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = {
  	
  
  	if(lst == Empty())
  		None
  	else if(maxHelper(lst, compare) == None.asInstanceOf[A]){
  		None
  	}
  	else{
  		Some(maxHelper(lst, compare))
  	}
  }

  def maxHelper[A](lst: JoinList[A], compare:(A,A) => Boolean) : A = lst match{
  	case Singleton(x) => x
  	case Join(lst1, lst2, _) => (lst1, lst2) match{
  		case (Empty(),Empty()) =>{
  			if(lst1.size != 0 && lst2.size != 0){
  				val a = maxHelper(restHelper(lst1), compare)
  				val b = maxHelper(restHelper(lst2), compare)
  				if(compare(a,b) == true)
  					a
  				else
  					b
  			}
  			else if(lst1.size != 0){
  				maxHelper(lst1, compare)
  			}
  			else if(lst2.size != 0){
  				maxHelper(lst2, compare)
  			}
  			else{
  				maxHelper(Empty(), compare)
  			}

  		}
  		case (Empty(),_) =>{
  			if(lst1.size == 0){
  				maxHelper(lst2, compare)
  			}
  			else{
  				val a = maxHelper(restHelper(lst1), compare)
  				val b = maxHelper(lst2, compare)
  				if(compare(a,b) == true)
  					a
  				else
  					b
  			}
  		}
  		case ( _, Empty()) =>{
  			if(lst2.size == 0){
  				maxHelper(lst1, compare)
  			}
  			else{
  				val a = maxHelper(lst1, compare)
  				val b = maxHelper(restHelper(lst2), compare)
  				if(compare(a,b) == true)
  					a
  				else
  					b
  			}
  		}
  		case (_,_) => {
  			val a = maxHelper(lst1, compare)
  			val b = maxHelper(lst2, compare)
  			if(compare(a,b) == true)
  				a
  			else
  				b
  		}
  	}
  	case Empty() => {
  		None.asInstanceOf[A]
  	}
  }

  def first[A](lst: JoinList[A]): Option[A] = lst match{
  	case Empty() => None
  	case Singleton(x) => Some(x)
  	case Join(lst1, lst2, _) =>{
  		if(first(lst1) == None && first(lst2) == None)
  			None
  		else if(first(lst1) == None)
  			first(lst2)
  		else
  			first(lst1)
  	}
  }

  def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match{
  	case Empty() => None
  	case _ => Some(restHelper(lst))
  }

  def restHelper[A](lst: JoinList[A]): JoinList[A] = lst match{
  	case Singleton(x) => Empty()
  	case Join(lst1, lst2, _) => {
  		if(lst1 == Empty()){
  			restHelper(lst2)
  		}
  		else if(lst1.size == 1){
  			Join(Empty(),lst2, lst2.size)
  		}
  		else{
  			Join(restHelper(lst1), lst2, restHelper(lst1).size + lst2.size)
  		}
  	}
  	case Empty() => Empty()
  }

  def nth[A](lst: JoinList[A], n: Int): Option[A] = {
  	nthHelper(lst, n, 0)
  }


  def nthHelper[A](lst: JoinList[A], n:Int, count:Int): Option[A] = {
  	if(n == count)
  		first(lst)
  	else
  		nthHelper(restHelper(lst),n,count+1)
  }



  def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match{
  	case Empty() => Empty()
  	case Singleton(x) => Singleton(f(x))
  	case Join(lst1, lst2, size) => {
  		if(first(lst1) == None && first(lst2) == None){
  			Join(map(f, restHelper(lst1)), map(f, restHelper(lst2)), size-2)
  		}
  		else if(first(lst1) == None){
  			Join(map(f, restHelper(lst1)), map(f, lst2), size-1)
  		}
  		else if(first(lst2) == None){
  			Join(map(f, lst1), map(f, restHelper(lst2)), size-1)
  		}
  		else
  			Join(map(f, lst1), map(f, lst2), size)
  	}
  }

  def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match{
  	case Empty() => Empty()
  	case Singleton(x) => {
  		if(pred(x) == true)
  			Singleton(x)
  		else
  			Empty()

  	}
  	case Join(lst1, lst2, _) => Join(filter(pred, lst1), filter(pred, lst2), filter(pred,lst1).size + filter(pred,lst2).size)
  }

}