object DateImplicits{

	import java.time._

	sealed trait TimeUnit

	case class Days(n: Int) extends TimeUnit{
		def getDays(): Int = n
	}

	case class Months(n: Int) extends TimeUnit{
		def getMonths(): Int = n
	}

	case class Years(n: Int) extends TimeUnit{
		def getYears(): Int = n
	}

	implicit class RichInt(n: Int){
		def jan() : LocalDate = LocalDate.now().withMonth(1).withDayOfMonth(n)
		def feb() : LocalDate = LocalDate.now().withMonth(2).withDayOfMonth(n)
		def mar() : LocalDate = LocalDate.now().withMonth(3).withDayOfMonth(n)
		def apr() : LocalDate = LocalDate.now().withMonth(4).withDayOfMonth(n)
		def may() : LocalDate = LocalDate.now().withMonth(5).withDayOfMonth(n)
		def jun() : LocalDate = LocalDate.now().withMonth(6).withDayOfMonth(n)
		def jul() : LocalDate = LocalDate.now().withMonth(7).withDayOfMonth(n)
		def aug() : LocalDate = LocalDate.now().withMonth(8).withDayOfMonth(n)
		def sep() : LocalDate = LocalDate.now().withMonth(9).withDayOfMonth(n)
		def oct() : LocalDate = LocalDate.now().withMonth(10).withDayOfMonth(n)
		def nov() : LocalDate = LocalDate.now().withMonth(11).withDayOfMonth(n)
		def dec() : LocalDate = LocalDate.now().withMonth(12).withDayOfMonth(n)

		def jan(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(1).withDayOfMonth(n)
		def feb(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(2).withDayOfMonth(n)
		def mar(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(3).withDayOfMonth(n)
		def apr(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(4).withDayOfMonth(n)
		def may(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(5).withDayOfMonth(n)
		def jun(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(6).withDayOfMonth(n)
		def jul(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(7).withDayOfMonth(n)
		def aug(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(8).withDayOfMonth(n)
		def sep(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(9).withDayOfMonth(n)
		def oct(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(10).withDayOfMonth(n)
		def nov(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(11).withDayOfMonth(n)
		def dec(yr: Int) : LocalDate = LocalDate.now().withYear(yr).withMonth(12).withDayOfMonth(n)

		def days() : Days = new Days(n)
		def months() : Months = new Months(n)
		def years() : Years = new Years(n)

	}

	implicit class RichLocalDate(local: LocalDate){
		def +(t: TimeUnit) : LocalDate = t match{
			case Days(n) => local.plusDays(n)
			case Months(n) => local.plusMonths(n)
			case Years(n) => local.plusYears(n)
		}
	}
}