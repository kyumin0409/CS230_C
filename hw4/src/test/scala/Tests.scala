class Tests extends org.scalatest.FunSuite {

  import FunctionalDataStructures._
  //import Solution._

  def fromList[A](lst: List[A]): JoinList[A] = lst match {
    case Nil => Empty()
    case List(x) => Singleton(x)
    case _  => {
      val len = lst.length
      val (lhs, rhs) = lst.splitAt(len / 2)
      Join(fromList(lhs), fromList(rhs), len)
    }
  }

  def toList[A](lst: JoinList[A]): List[A] = lst match {
    case Empty() => Nil
    case Singleton(x) => List(x)
    case Join(lst1, lst2, _) => toList(lst1) ++ toList(lst2)
  }

  test("enqueue test"){
    val q1 = new Queue(List(1,2,3),List(6,5,4))
    val q2 = new Queue(List("a","b","c"),List("g","f","e","d"))

    assert(enqueue(0,q1) == new Queue(List(1,2,3),List(0,6,5,4)))
    assert(enqueue("j",q2) == new Queue(List("a","b","c"), List("j","g","f","e","d")))
  }

  test("dequeue test"){
    val q1 = new Queue(List(1,2,3),List(6,5,4))
    val q2 = new Queue(List("a","b","c"),List("g","f","e","d"))

    assert(dequeue(q1) == Some((1, new Queue(List(2,3),List(6,5,4)))))
    assert(dequeue(q2) == Some(("a", new Queue(List("b","c"), List("g","f","e","d")))))
  }

  test("max test"){
    val lst1 = List(1,2,3,4,5,6,7,0,3,6)
    val jl1 = fromList(lst1)

    val lst2 = List(9,0,4,2,7,1,4,4,4)
    val jl2 = fromList(lst2)

    val lst3 = List()
    val jl3 = fromList(lst3)

    val lst5 = List(0,0,1)
    val jl5 = fromList(lst5)

    def compInt (a: Int, b: Int) : Boolean ={
      if(a>= b)
        true
      else
        false
    }

    val jl4 = Join(Join(Empty(), Singleton(1), 1), Join(Singleton(4), Singleton(3), 2),3)

    assert(max(jl1, compInt) == Some(7))
    assert(max(jl2, compInt) == Some(9))
    assert(max(jl3, compInt) == None)
    assert(max(jl4, compInt) == Some(4))
    assert(max(jl5, compInt) == Some(1))
  }

  test("first test"){
    val lst1 = List()
    val jl1 = fromList(lst1)

    val lst2 = List(3,5,1,1,2)
    val jl2 = fromList(lst2)

    val lst3 = List("B","E","C","A")
    val jl3 = fromList(lst3)

    assert(first(jl1) == None)
    assert(first(jl2) == Some(3))
    assert(first(jl3) == Some("B"))
    assert(first(Join(Empty(),Singleton(2),1)) == Some(2))
  }

  test("rest test"){
    val lst1 = List[Int]()
    val jl1 = fromList(lst1)

    val lst2 = List(1)
    val jl2 = fromList(lst2)

    val lst3 = List(1,2,3,4,5)
    val jl3 = fromList(lst3)

    val lst4 = List(2,3,4,5)
    val jl4 = fromList(lst4)

    //println(jl3)

    assert(rest(jl1) == None)
    assert(rest(jl2) == Some(Empty()))
    assert(toList(restHelper(jl3)) == lst4)


    assert(restTest(jl1) == true)
    assert(restTest(jl2) == true)
    assert(restTest(jl3) == true)
    assert(restTest(jl4) == true)
  }

  def mapTest[A, B] (f: A=>B, alist: List[A]) : Boolean = {
    toList(map(f, fromList(alist))) == alist.map(f)
  }

  def restTest[A](jlist: JoinList[A]): Boolean = {
    val alist = toList(jlist)
    rest(jlist) match{
      case None => alist == Nil
      case Some(jlistRest) => toList(jlistRest) == alist.tail
    }
  }


  test("nth test"){
    val lst1 = List[Int]()
    val jl1 = fromList(lst1)

    val lst2 = List(1)
    val jl2 = fromList(lst2)

    val lst3 = List(1,2,3,4,5)
    val jl3 = fromList(lst3)

    assert(nth(jl1,0) == None)
    assert(nth(jl1,1) == None)
    assert(nth(jl2,0) == Some(1))
    assert(nth(jl2,1) == None)
    assert(nth(jl3,3) == Some(4))
    assert(nth(jl3,5) == None)
    assert(nth(jl3,2) == Some(3))
  }

  test("map test"){
    def f1(a: Int) : Int ={
      a+1
    }
    def f2(a: String) : Int ={
      a.length
    }


    val lst1 = List[Int]()
    val jl1 = fromList(lst1)

    val lst2 = List(1)
    val jl2 = fromList(lst2)

    val lst3 = List(1,2,3,4,5)
    val jl3 = fromList(lst3)

    val lst4 = List(2,3,4,5,6)
    val jl4 = fromList(lst4)

    val lst5 = List("A", "AB", "ABC", "ABCD", "ABCDE")
    val jl5 = fromList(lst5)

    val jl6 = Join(Join(Empty(), Singleton(1), 1), Join(Singleton(4), Singleton(3), 2),3)
    val jl7 = Join(Join(Empty(), Singleton(1), 1), Join(Empty(), Singleton(3), 1),2)


    assert(toList(map(f1, jl3)) == lst4)
    assert(toList(map(f1, jl1)) == lst1)
    assert(toList(map(f2, jl5)) == lst3)
    assert(toList(map(f1, jl6)) == List(2,5,4))
    assert(toList(map(f1, jl7)) == List(2,4))
  }

  test("filter test"){
    def p(a: Int) : Boolean ={
      if(a%2==0)
        true
      else
        false
    }

    val lst1 = List[Int]()
    val jl1 = fromList(lst1)

    val lst2 = List(1)
    val jl2 = fromList(lst2)

    val lst3 = List(1,2,3,4,5)
    val jl3 = fromList(lst3)

    val jl4 = Join(Join(Empty(), Singleton(1), 1), Join(Singleton(4), Singleton(3), 2),3)


    assert(toList(filter(p,jl1)) == List())
    assert(toList(filter(p,jl2)) == List())
    assert(toList(filter(p,jl3)) == List(2,4))
    assert(toList(filter(p,jl4)) == List(4))
  }

}