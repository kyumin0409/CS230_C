import hw.generics._

sealed trait BinTree[A] extends ListLike[A, BinTree[A]]
case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A]{
	def cons(head: A) : BinTree[A] = Node(Leaf(), head, this)
	def head () : Option[A] = 
		lhs.head match {
			case None => Some(value)
			case Some(head) => Some(head)
		}
	def isEmpty() : Boolean = false
	def tail() : Option[BinTree[A]] = 
		lhs.tail match {
			case None => Some(rhs)
			case Some(tail) => Some(Node(tail, value, rhs))
		}
}
case class Leaf[A]() extends BinTree[A]{
	def cons(value: A) : BinTree[A] = Node(Leaf(), value, Leaf())
	def head() : Option[A] = None
	def isEmpty() : Boolean = true
	def tail() : Option[BinTree[A]] =None
}


object ListFunctions{
	def filter[A, L <: ListLike[A,L]](f:A => Boolean , alist: L) : L = {
		filterHelper[A,L](alist) match{
			case None => alist
			case Some((head,tail)) =>{
				if(f(head) == true){
					filter(f,tail).cons(head)
				}
				else{
					filter(f,tail)
				}
			}
		}
	}

	def filterHelper[A, L <: ListLike[A,L]](alist: L) : Option[(A,L)] = {
		(alist.head(), alist.tail()) match{
			case(None, None) => None
			case(Some(head),Some(tail)) => Some((head,tail))
			case _ => sys.error("constructed to be not working listlike")
		}
	}

	def append[A,L <: ListLike[A,L]](alist1: L, alist2: L) : L = {
		if(filterHelper[A,L](alist1) == None){
			filterHelper[A,L](alist2) match {
				case (None) => alist2
				case Some((head,tail)) => append[A,L](alist1, tail).cons(head)
			}
		}
		else{
			filterHelper[A,L](alist1) match{
				case (None) => append[A,L](alist1,alist2)
				case Some((head,tail)) => append[A,L](tail, alist2).cons(head)
			}
		}
	}

	/*case class OInt[A<:Int](x: A) extends Ordered[A]{
		def compare (other:A): Ordering = {
			if(x>other) GT
			else if(x == other) EQ
			else LT
		}
	}*/

	def sortHelper[A <: hw.generics.Ordered[A], L <: ListLike[A,L]](a: A, alist: L): L = {
		filterHelper[A,L](alist) match {
			case None => alist.cons(a)
			case Some((head,tail)) => {
				head.compare(a) match{
					case LT => sortHelper(head,tail).cons(a)
					case EQ => sortHelper(a,tail).cons(head)
					case GT => sortHelper(a,tail).cons(head)
				}
			}
		}
	}

	def sort[A <: hw.generics.Ordered[A],L <: hw.generics.ListLike[A,L]](alist: L): L = {
		filterHelper[A,L](alist) match{
			case None => alist
			case Some((head,tail)) => sortHelper[A,L](head,sort[A,L](tail))
		}
	}



}

class C1 extends T2[Int,Int,String,String] with T3[Int,Int,Int,String,String,String,Int]{
	def f(a: Int, b:Int) : Int = 0
	def g(c: String) : String = ""
	def h(d: String) : Int = 0
}

class C2 extends T1[Int,Int] with T2[Int,Int,Int,Int] with T3[Int,Int,Int,Int,Int,Int,Int]{
	def f(a: Int, b: Int): Int = 0
	def g(c: Int): Int = 0
	def h(d: Int): Int = 0
}

class C3[A](x: A) extends T3[Int,A,Int,A,String,String,A]{
	def f(a: Int, b: A): Int = 0
	def g(c: A) : String = ""
	def h(d: String): A = x
}

class C4[A](x: Int, y: C4[A]) extends T1[Int,C4[A]] with T3[Int,C4[A],C4[A],Int,C4[A],C4[A],Int]{
	def f(a: Int, b: C4[A]): C4[A] = b
	def g(c: Int): C4[A] = y
	def h(d: C4[A]): Int = x
}