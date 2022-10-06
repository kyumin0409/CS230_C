// You may modify or delete this file
import Homework3._

class TestSuite extends org.scalatest.FunSuite {

  import edu.umass.cs.CSV

 val lifeExpectancy = CSV.fromFile("cdc-life-expectancy.csv")
 val allBirths = CSV.fromFile("shorter-ssa-birth.csv")


  test("yearIs test"){
  	assert(yearIs(lifeExpectancy, 1997) == List(List("1997", "74", "79")))
 	assert(yearIs(allBirths, 1994) == List(List("1994","Jessica","F","32116"),List("1994","Ashley","F","30277"),List("1994","Emily","F","24148")))
  }

  test("yearGT test"){
  	assert(yearGT(lifeExpectancy, 2007) == List(List("2010", "76", "81"), List("2009", "75", "80"), List("2008", "75", "80")))
  	assert(yearGT(allBirths,2000) == List(List("2001","Caleb","M","6021"),List("2001","Jessie","F","35")))
  }

  test("yearLT test"){
  	assert(yearLT(lifeExpectancy, 1934) == List(List("1933","58","62"), List("1932","58","62"), List("1931","58","62"), List("1930","58","62")))
  	assert(yearLT(allBirths,1994) == List(List("1993","Jude","M","2564")))
  }

  test("onlyName test"){
  	assert(onlyName(allBirths, "Jessica") == List(List("1994","Jessica","F","32116"),List("1995","Jessica","F","301"),List("1997","Jessica","F","9034")))
  	/*intercept[Exception]{
  		onlyName(lifeExpectancy,"Wood")
  	}*/
  }

  test("mostPopular test"){
  	assert(mostPopular(allBirths) == ("Jessica", 32116))
  	//assert(mostPopular(List(List())) == ("",0))
  	/*intercept[Exception]{
  		mostPopular(lifeExpectancy)
  	}*/
  }

  test("count test"){
  	assert(count(allBirths) == 107749)
  	/*intercept[Exception]{
  		count(lifeExpectancy)
  	}*/
  }

  test("countGirlsAndBoys test"){
  	assert(countGirlsAndBoys(allBirths) == (96155,11594))
  	/*intercept[Exception]{
  		countGirlsAndBoys(lifeExpectancy)
  	}*/
  }

  test("unisexNames test"){
  	assert(unisexNames(allBirths) == Set("Parker", "Jude"))
  	assert(unisexNames(allBirths) == Set("Jude", "Parker"))
  	/*intercept[Exception]{
  		unisexNames(lifeExpectancy)
  	}*/
  }

  test("expected alive test"){
  	assert(expectedAlive("M", 1948, 1950) == true)
  	assert(expectedAlive("M", 1948, 2009) == true)
  	assert(expectedAlive("M", 1948, 2010) == false)
  	assert(expectedAlive("F", 1953, 1955) == true)
  	assert(expectedAlive("F", 1953, 2024) == true)
  	assert(expectedAlive("F", 1953, 2025) == false)
  	assert(expectedAlive("F", 2010, 2001) == false)
  }

  test("estimatePopulation test"){
  	assert(estimatePopulation(allBirths,2000) == 101693)
  	assert(estimatePopulation(allBirths,2004) == 107749)
  	assert(estimatePopulation(allBirths,2065) == 107749)
  	assert(estimatePopulation(allBirths,2074) == 18644)
  	assert(estimatePopulation(allBirths,2081) == 35)
  	assert(estimatePopulation(allBirths,2082) == 0)
  }

  test("Have life expectancies from 1930 -- 2010") {
    assert(CSV.fromFile("cdc-life-expectancy.csv").map(x => x(0).toInt).reverse
           == 1930.to(2010))
  }


}