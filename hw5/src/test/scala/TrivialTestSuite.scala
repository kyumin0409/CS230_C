import Solution._

class TrivialTestSuite extends org.scalatest.FunSuite {

  test("The solution object must be defined") {
    val obj : MinimaxLike = Solution
  }

  test("isFinished test"){
  	val g1 = createGame(X,3,Map())
  	assert(g1.isFinished() == false)
  	val g2 = createGame(X,2,Map((0,0)->X,(0,1)->O,(1,0)->O,(1,1)->X))
  	assert(g2.isFinished() == true)
  }

  test("getWinner test"){
  	val g1 = createGame(X,2,Map((0,0)->X,(0,1)->O,(1,0)->O,(1,1)->X))
  	assert(g1.getWinner() != None)
  	val g2 = createGame(X,3,Map((0,0)->X,(0,1)->X,(0,2)->X,(1,0)->X,(1,1)->O,(1,2)->X,(2,0)->O,(2,1)->X,(2,2)->O))
  	assert(g2.getWinner() == Some(X))
  }

  test("nextBoards test"){
  	val g1 = createGame(X,3,Map())
  	assert(g1.nextBoards().size == 9)
  	val g2 = createGame(X,2,Map((0,0)->X,(0,1)->O,(1,0)->O,(1,1)->X))
  	assert(g2.nextBoards().size == 0)
  	val g3 = createGame(X,2,Map((0,0)->X,(0,1)->O,(1,0)->O))
  	assert(g3.nextBoards().size == 1)
  }

  test("minimax test"){
  	val g1 = createGame(X,3,Map((0,0)->X,(0,1)->X,(0,2)->O,(1,0)->X,(1,1)->O,(1,2)->X,(2,1)->X,(2,2)->O))
  	assert(minimax(g1) == Some(X))
  	val g2 = createGame(O,3,Map((0,0)->X,(0,1)->X,(0,2)->O,(1,0)->X,(1,1)->O,(1,2)->X,(2,1)->X,(2,2)->O))
  	assert(minimax(g2) == Some(O))
  	val g3 = createGame(O,3,Map((0,0)->X,(0,1)->O,(0,2)->X,(1,0)->O,(1,1)->X,(1,2)->O,(2,0)->O,(2,1)->X))
  	assert(minimax(g3) == None)
  }
}