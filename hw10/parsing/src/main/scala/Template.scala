import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike{
	def eval(e: Expr) : Double = e match {
		case Num(n) => n
		case Exponent(e1, e2) => math.pow(eval(e1), eval(e2))
		case Div(e1, e2) => eval(e1) / eval(e2)
		case Mul(e1, e2) => eval(e1) * eval(e2)
		case Add(e1, e2) => eval(e1) + eval(e2)
		case Sub(e1, e2) => eval(e1) - eval(e2)

	}
	
}

object ArithParser extends ArithParserLike{

	//number: PackratParser[Double] is defined in ArithParserLike

	//lexical.mkLi ++= List("+","*","-","/","(",")")



	lazy val atom: PackratParser[Expr] = (ArithParser.number ^^ {x=>Num(x)}) | ("("~>expr<~")")  | (expr)

	lazy val exponent: PackratParser[Expr] = ((exponent~"^"~atom) ^^ {case(a~"^"~b) => Exponent(a,b)}) | (atom)

	lazy val add: PackratParser[Expr] = ((mul~"+"~add) ^^ {case(a~"+"~b) => Add(a,b)}) | ((mul~"-"~add) ^^ {case(a~"-"~b) => Sub(a,b)}) | (mul)

	lazy val mul: PackratParser[Expr] = ((exponent~"*"~mul) ^^ {case(a~"*"~b) => Mul(a,b)}) | ((exponent~"/"~mul) ^^ {case(a~"/"~b) => Div(a,b)}) | (exponent)

	lazy val expr: PackratParser[Expr] = add
}

object ArithPrinter extends ArithPrinterLike {
	def print(e: Expr): String = e match{
		case Num(n) => s"$n"
		case Exponent(e1,e2) => {
				val a = print(e1)
				val b = print(e2)
		    	s"($a^$b)"
		 	}
		 case Div(e1,e2) => {
				val a = print(e1)
				val b = print(e2)
		    	s"($a/$b)"
		 	}
		 case Mul(e1,e2) => {
				val a = print(e1)
				val b = print(e2)
		    	s"($a*$b)"
		 	}
		 case Add(e1,e2) => {
				val a = print(e1)
				val b = print(e2)
		    	s"($a+$b)"
		 	}
		 case Sub(e1,e2) => {
				val a = print(e1)
				val b = print(e2)
		    	s"($a-$b)"
		 	}
	}
}