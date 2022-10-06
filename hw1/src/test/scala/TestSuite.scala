import Lecture1._

class TestSuite extends org.scalatest.FunSuite {
	
	test("oddNumbers properly defined"){
		assert(oddNumbers == List(1,3,5))
	}

	test("sumDouble test"){
		assert(sumDouble(oddNumbers) == 18)
		assert(sumDouble(List(1,0,5,8,2)) == 32)
		assert(sumDouble(List(10,2,-1,6,-2)) == 30)
		assert(sumDouble(Nil) == 0)
	}

	test("removeZeroes test"){
		assert(removeZeroes(List[Int](1,0,2,0,3,0,4)) == List[Int](1,2,3,4))
		assert(removeZeroes(List[Int](0,0,0)) == Nil)
		assert(removeZeroes(List[Int](0,0,1)) == List[Int](1))
		assert(removeZeroes(List(1,10,3,5,2)) == List(1,10,3,5,2))
		assert(removeZeroes(List(1,0,3,5,2)) == List(1,3,5,2))
		assert(removeZeroes(Nil) == Nil)
	}

	test("countEvens test"){
		assert(countEvens(List[Int](1,2,3,4)) == 2)
		assert(countEvens(List[Int](0,2,0,2,3)) == 4)
		assert(countEvens(List(5,2,-2,1,0)) == 3)
		assert(countEvens(Nil) == 0)
	}

	test("removeAlternating test"){
		assert(removeAlternating(List("A", "B", "C", "D")) == List("A", "C"))
		assert(removeAlternating(List("A", "A", "B", "C", "D")) == List("A", "B", "D"))
		assert(removeAlternating(Nil) == Nil)
		assert(removeAlternating(List("A")) == List("A"))
		assert(removeAlternating(List("A", "B")) == List("A"))
		assert(removeAlternating(List("A", "B")) != List("B"))
	}

	test("isAscending test"){
		assert(isAscending(List(1,1,3,4,5,6))==true)
		assert(isAscending(List())==true)
		assert(isAscending(List(1,1,0))==false)
		assert(isAscending(List(3,5,4,6,8))==false)
		assert(isAscending(List(3)) == true)
		assert(isAscending(List(3,2)) == false)
		assert(isAscending(List(1,3))==true)

	}

	test("addSub test"){
		assert(addSub(List(10,20,30,40)) == -20)
		assert(addSub(List(0,1,2,3,4))==2)
		assert(addSub(List(4,-2,8,3,1))==12)
	}

	test("alternate test"){
		assert(alternate(List(1,3,5), List(2,4,6)) == List(1,2,3,4,5,6))
		assert(alternate(Nil, Nil) == Nil)
		assert(alternate(List(2,8,3,6), List(1,5,34,2)) == List(2,1,8,5,3,34,6,2))
	}

	test("fromTo test"){
		assert(fromTo(9,13) == List(9, 10, 11, 12))
		assert(fromTo(-3,7) == List(-3,-2,-1,0,1,2,3,4,5,6))
		assert(fromTo(2,1) == Nil)
		assert(fromTo(4,4) == Nil)
	}

	test("insertOrdered test"){
		assert(insertOrdered(5, List(1,3,7,9)) == List(1,3,5,7,9))
		assert(insertOrdered(1, List(2,3,4,5)) == List(1,2,3,4,5))
		assert(insertOrdered(7, List(1,2,3,4,5)) == List(1,2,3,4,5,7))
		assert(insertOrdered(5, List(1,3,5,5,6,7)) == List(1,3,5,5,5,6,7))
		intercept[Exception]{
			insertOrdered(5, List(1,7,3,9))
		}
		assert(insertOrdered(4, Nil) == List(4))
	}

	test("sort test"){
		assert(sort(List(3, 1, 6, 2, 5, 8)) == List(1, 2, 3, 5, 6, 8))
		assert(sort(List(6, 80, -23, 51, 0)) == List(-23, 0, 6, 51, 80))
		assert(sort(List(1)) == List(1))
		assert(sort(List(1,2)) == List(1,2))
		assert(sort(List(2,1)) == List(1,2))
		assert(sort(Nil) == Nil)
	}
}