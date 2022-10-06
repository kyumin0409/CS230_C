
object Excercises{
	
	sealed trait Bird
	case class Duck() extends Bird
	case class Goose() extends Bird

	def map[A,B](f: A => B, xs: List[A]): List[B] = xs match{
		case Nil =>Nil
		case head :: tail => f(head) :: map[A,B](f, tail)
	}

	

	def excercise1(lst: List[Bird]) : List[String]= {
		def f(bird: Bird): String = {
			if(bird == Duck())
				"dog food"
			else
				"pate"
		}
		map(f, lst)
	}

	def fold[A,B](acc: B, f: (B,A) => B, xs: List[A]) : B ={
		xs match{
			case Nil => acc
			case h :: tail => fold(f(acc,h), f, tail)
		}
	}

	


	def excercise3(lst: List[Bird]): Int = {
		
		def add (i:Int, b:Bird) : Int ={
			if(b==Duck())
				i +1
			else
				i +10
		}

		fold(0, add, lst)
	}
}