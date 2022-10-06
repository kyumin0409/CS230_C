import java.util.UUID

class CandyDatabase(candies: Map[String, BigDecimal]) {

  def f(lst: List[String]) : List[(String,UUID)] = lst match{
  	case Nil => Nil
  	case head :: tail => (head, UUID.randomUUID) :: f(tail)
  }

  val barcodes : Map[String,UUID] = {
  	f(candies.keys.toList).toMap
  }

  /*val barcodes : Map[String,UUID] = Map(
  	"Snozzberries" -> UUID.randomUUID(),
  	"Everlasting Gobstopper" -> UUID.randomUUID(),
  	"Fizzy Lifting Drink" -> UUID.randomUUID(),
  	"Edible Teacup" -> UUID.randomUUID(),
  	"Wonka Bar" -> UUID.randomUUID()
  )*/

  def f2(lst: List[String]) : List[(UUID,BigDecimal)] = lst match{
  	case Nil => Nil
  	case head :: tail =>{
  		val id = barcodes(head)
  		(id, candies(head)) :: f2(tail)
  	}
  }

  val prices : Map[UUID,BigDecimal] = {
  	f2(barcodes.keys.toList).toMap
  }
  /*val prices : Map[UUID,BigDecimal] = Map(
  	barcodes("Snozzberries") -> BigDecimal(2.49),
  	barcodes("Everlasting Gobstopper") -> BigDecimal(0.99),
  	barcodes("Fizzy Lifting Drink") -> BigDecimal(1.99),
  	barcodes("Edible Teacup") -> BigDecimal(4.79),
  	barcodes("Wonka Bar") -> BigDecimal(1.50)
  )*/


  def getPriceFromBarcode(b: UUID) : Option[BigDecimal] = {
  	if (prices.contains(b)){
  		Some(prices(b))
  	}
  	else{
  		None
  	}
  }
  def getBarcodeForCandy(c: String) : Option[UUID] = {
  	if (barcodes.contains(c)){
  		Some(barcodes(c))
  	}
  	else{
  		None
  	}
  }
}