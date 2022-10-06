object Homework3 {

  import edu.umass.cs.CSV


  // WARNING: this may take a very long time. Cut the file or work with a
  // small, made-up dataset if you have trouble.
  // val allBirths = CSV.fromFile("ssa-births.csv")

  val lifeExpectancy = CSV.fromFile("cdc-life-expectancy.csv")

  /** Restrict the data to the year `year`. */
  def yearIs(data: List[List[String]], n: Int): List[List[String]] = data match{
  	case Nil => Nil
  	case head :: tail => {
  		if(head(0) == n.toString)
  			head :: yearIs(tail,n)
  		else
  			yearIs(tail,n)
  	}
  }


  /** Restrict the data to years greater than `bound`. */
  def yearGT(data: List[List[String]], bound: Int): List[List[String]] = data match{
  	case Nil => Nil
  	case head :: tail => {
  		if(head(0) > bound.toString)
  			head :: yearGT(tail,bound)
  		else
  			yearGT(tail,bound)
  	}
  }

  /** Restrict the data to years less than `bound` */
  def yearLT(data: List[List[String]], bound: Int): List[List[String]] = data match{
  	case Nil => Nil
  	case head :: tail => {
  		if(head(0) < bound.toString)
  			head :: yearLT(tail,bound)
  		else
  			yearLT(tail,bound)
  	}
  }

  /** Restrict the data to the name `name`. */
  def onlyName(data: List[List[String]], name: String): List[List[String]] = data match{
  	case Nil => Nil
  	case head :: tail => {
  		//if(head.length != 4)
  		//	sys.error("not the right list")
  		if(head(1) == name)
  			head :: onlyName(tail, name)
  		else
  			onlyName(tail, name)
  	}
  }

  /** Calculate the most popular name and the number of children born with
      that name. */
  def mostPopular(data: List[List[String]]): (String, Int) = {
  	mostPopularHelper(data, 0, "")
  }

  def mostPopularHelper(data: List[List[String]], num: Int, name: String): (String, Int) = data match{
  	case Nil => (name, num)
  	case head :: tail => {
  		//if(head.length != 4)
  		//	sys.error("not the right list")
  		if(head(3).toInt > num)
  			mostPopularHelper(tail, head(3).toInt, head(1))
  		else
  			mostPopularHelper(tail, num, name)
  	}
  }
  

  /** Calculate the number of children born in the given dataset. */
  def count(data: List[List[String]]): Int = data match{
  	case Nil => 0
  	case head :: tail =>{
  		//if(head.length != 4)
  		//	sys.error("not the right list")
  		head(3).toInt + count(tail)
  	}
  }


  /** Produce a tuple with the number of girls and boys respectively. */
  def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = {
  	(countGirls(data), count(data) - countGirls(data))
  }

  def countGirls(data: List[List[String]]): Int = data match{
  	case Nil => 0
  	case head :: tail =>{
  		//if(head.length != 4)
  		//	sys.error("not the right list")
  		if(head(2) == "F")
  			head(3).toInt + countGirls(tail)
  		else
  			countGirls(tail)
  	}
  }
  

  /** Calculate the set of names that are given to both girls and boys. */
  def unisexNames(data: List[List[String]]): Set[String] = data match{
  	case Nil => Set()
  	case head :: tail =>{

  		//if(head.length != 4)
  		//	sys.error("not the right list")

  		def findUnisex (lst: List[List[String]], name: String, gender: String) : Set[String]= lst match{
  			case Nil => Set()
  			case head :: tail =>{
  				if(gender == "F")
  					if(head(2) == "M")
  						Set(name)
  					else
  						findUnisex(tail, name, gender)
  				else
  					if(head(2) == "F")
  						Set(name)
  					else
  						findUnisex(tail, name, gender)
  			}
  		}

  		findUnisex(onlyName(data, head(1)), head(1), head(2)).union(unisexNames(tail))

  	}
  }



  /** Determine if a person with the specified `gender` (either "M" or "F") who
      was born in `birthYear` is expected to be alive, according to the CDC
      life-expectancy data.

      If `currentYear` is the last year the person is estimated to be alive, be
      optimistic and produce `true`.

      The CDC data only ranges from 1930 -- 2010. Therefore, assume that
      `birthYear` is in this range too. */
  def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = {
  	//list = yearIs(lifeExpectancy, birthYear)
  	if(currentYear < birthYear) 
  		false

  	else if(gender == "M"){
  		if(birthYear + yearIs(lifeExpectancy, birthYear).head(1).toInt < currentYear)
  			false
  		else
  			true
  	}else{
  		if(birthYear + yearIs(lifeExpectancy, birthYear).head(2).toInt < currentYear)
  			false
  		else
  			true
  	}
  }

  /** Estimate how many people from `data` will be alive in `year`. */
  def estimatePopulation(data: List[List[String]], year: Int): Int = data match{
  	case Nil => 0
  	case head :: tail =>{
  		if(expectedAlive(head(2), head(0).toInt, year) == true)
  			head(3).toInt + estimatePopulation(tail, year)
  		else
  			estimatePopulation(tail, year)
  	}
  }

}