import Excercises._

class ExcerciseTests extends org.scalatest.FunSuite{

	test("excercise 1 test"){
		assert(excercise1(List()) == List())
		assert(excercise1(List(Duck(),Duck(),Goose())) == List("dog food", "dog food", "pate"))
	}

	test("excercise 3 test"){
		assert(excercise3(List(Duck(), Duck(), Goose())) == 12)
		assert(excercise3(List(Goose(), Goose(), Goose(), Goose(), Duck(), Goose())) == 51)
		assert(excercise3(List()) == 0)
	}
}