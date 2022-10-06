import Homework2._

class TestSuite extends org.scalatest.FunSuite{
	
	test("map2 with add"){
		def add(x: Int, y: Int): Int = x+y
		assert(map2(add, List(1,2,3), List(4,5,6)) == List(5,7,9))
		intercept[Exception]{
			map2(add, List(1,2), List(2,3,4))
		}
	}

	test("map2 test 1"){
		def strLength(s1: String, s2: String): Int = s1.length + s2.length
		assert(map2(strLength, List("hello", "bye", "hi"), List("violin", "viola", "cello")) == List(11, 8, 7))
	}

	test("map2 test 2"){
		def addLength(s: String, x: Int): Double = s.length + x
		assert(map2(addLength, List("hello", "bye", "hi"), List(3,6,2)) == List(8.0, 9.0, 4.0))
	}

	test("zip test 1"){
		assert(zip(List(1,2,3), List(4,5,6)) == List((1,4), (2,5), (3,6)))

	}

	test("zip test 2"){
		assert(zip(List("George", "Teddy"), List("Washington", "Roosevelt")) == List(("George", "Washington"), ("Teddy", "Roosevelt")))
	}

	test("zip test 3"){
		assert(zip(List(1,2,3), List("A", "B", "C")) == List((1,"A"), (2, "B"), (3, "C")))
	}

	test("zip test 4"){
		intercept[Exception]{
			zip(List(1,2,3), List(2,3))
		}
	}

	test("flatten test"){
		assert(flatten(List(List(1,2), List(3,4))) == List(1,2,3,4))
		assert(flatten(List(List(1,2), List(3,4,5))) == List(1,2,3,4,5))
		assert(flatten(List(List(1,2,5), List(3,4))) == List(1,2,5,3,4))
		assert(flatten(List(List("A","B"), List("C","D"))) == List("A","B","C","D"))
	}

	test("flatten3 test"){
		assert(flatten3(List(List(List(1,2,3), List(4,5,6)), List(List(7,8,9), List(10,11,12)))) == List(1,2,3,4,5,6,7,8,9,10,11,12))
		assert(flatten3(List(List(List(1,2), List(4,5,6)), List(List(7,9), List(10,11,12)))) == List(1,2,4,5,6,7,9,10,11,12))
		assert(flatten3(List(List(List(1,2,3), List(4,5,6)), List(List(7,8,9), List(11,12)))) == List(1,2,3,4,5,6,7,8,9,11,12))
		assert(flatten3(List(List(List("A"), List()), List(List("B","C"), List("D", "E", "F")))) == List("A","B","C","D","E","F"))
	}

	test("buildList test 1"){
		def f(x: Int) = x
		assert(buildList(10, f) == List(0,1,2,3,4,5,6,7,8,9))
	}

	test("buildList test 2"){
		def f(x: Int) = 2*x
		assert(buildList(5, f) == List(0,2,4,6,8))
	}

	test("buildList test 3"){
		def f(x: Int) : Double = x
		assert(buildList(3, f) == List(0.0, 1.0, 2.0))
	}

	test("mapList test"){
		def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
		assert(mapList(List(1,2,3), f) == List(1,2,2,3,3,3))
		assert(mapList(List(4,5), f) == List(4,4,4,4,5,5,5,5,5))
	}

	def isEven(x: Int): Boolean ={(x%2) == 0}

	test("partition test 1"){
		assert(partition(isEven, List(1,2,3,4,5,6)) == (List(2,4,6), List(1,3,5)))
	}

	test("partition test 2"){
		assert(partition(isEven, List(2,4,6)) == (List(2,4,6), Nil))
	}

	test("partition test 3"){
		assert(partition(isEven, List(1,3,5)) == (Nil, List(1,3,5)))
	}

	test("partition test 4"){
		def oneLetter(str: String): Boolean = {str.length==1}
		assert(partition(oneLetter, List("A", "AB", "abc", "D")) == (List("A", "D"), List("AB", "abc")))
	}
}