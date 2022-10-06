import hw.sudoku._
import Solution._

/*class TestSuite extends org.scalatest.FunSuite{
	test("parse test"){
		def deParseHelper(cells: List[(Int,Int)], s: String, b: Board): String = cells match{
			case Nil => s+""
			case pos :: rest => {
				if(b.available(pos).length >1){
					deParseHelper(rest,s+'.',b)
				}
				else{
					val character = b.available(pos)(0).toString
					deParseHelper(rest,s+character,b)
				}
			}
		}

		def deParse(b:Board): String = {
			val keys = b.available.keys.toList.sorted
			deParseHelper(keys,"",b)
		}

		val str = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."

		assert(str.length == 81)
		//assert(deParse(parse(str)) == str)
	}
}*/

class TrivialTestSuite extends org.scalatest.FunSuite {
	test("The solution object must be defined"){
		val obj: hw.sudoku.SudokuLike = Solution
	}
}