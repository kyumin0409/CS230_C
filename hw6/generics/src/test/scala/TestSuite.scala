import ListFunctions._
import hw.generics._

class TestSuite extends org.scalatest.FunSuite{
	//1,2,3
	val bt1 = Node(Node(Leaf(),1,Leaf()),2,Node(Leaf(),3,Leaf()))
	val bt2 = Node(Node(Leaf(),4,Leaf()),5,Node(Leaf(),6,Leaf()))

	test("print bt1"){
		println(bt1)
	}

	test("insert test"){
		def in(a: Int, alist: List[Int]) : List[Int]= alist match {
			case Nil => a :: alist
			case head :: tail =>  {
				if(a<head){
					a :: in(head,tail)
				}
				else{
					head :: in(a,tail)
				}
			}
		}
		


		val lst = List(1,5,6,2,0)
		println(in(3,lst))

		def s1(alist: List[Int]) : List[Int] = alist match{
			case Nil => alist
			case head :: tail => in(head,s1(tail))
		}

		println(s1(lst))

		println(s1(List(9,40,19,3,-5,0,6,84)))
	}

	test("filter test"){
		def f(x:Int) : Boolean = {
			if(x%2 == 0) true
			else false
		}

		println(filter[Int,BinTree[Int]](f,bt1))
	}

	test("append test"){
		println(append[Int,BinTree[Int]](bt1,bt2))
		println(append[Int,BinTree[Int]](bt2,bt1))
	}

	test("sort test"){

	case class OInt[A<:Int](x: A) extends Ordered[A]{
		def compare (other:A): Ordering = {
			if(x>other) GT
			else if(x == other) EQ
			else LT
		}
	}



		val bt3 = Node(Leaf(),OInt[Int](1), Node(Leaf(),OInt[Int](2), Node(Leaf(),OInt[Int](3),Leaf())))
		//sort[OInt[Int],BinTree[OInt[Int]]](bt3)
	}

}