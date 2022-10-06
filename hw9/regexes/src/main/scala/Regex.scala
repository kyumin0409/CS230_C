import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike {
	def notAlphanumeric : Regex = "([^a-z][^A-Z][^0-9])*".r
	def time : Regex = "(2[0-3]|[0-1][0-9]):([0-5][0-9])".r
	def phone : Regex = "\\([0-9][0-9][0-9]\\)\\ [0-9][0-9][0-9]\\-[0-9][0-9][0-9][0-9]".r
	def zip : Regex = "([0-9][0-9][0-9][0-9][0-9])(\\-[0-9][0-9][0-9][0-9])?".r
	def comment : Regex = "/\\*(.*)\\*/".r
	def numberPhrase : Regex = {
		"((twenty)|(thirty)|(forty)|(fifty)|(sixty)|(seventy)|(eighty)|(ninety))((-one)|(-two)|(-three)|(-four)|(-five)|(-six)|(-seven)|(-eight)|(-nine))?".r
	}
	def roman : Regex = {
		"(((X)|(XX)|(XXX))?((I)|(II)|(III)|(IV)|(V)|(VI)|(VII)|(VIII)|(IX)))|(()|(X)|(XX)|(XXX)|(XL))".r
	}
	def date : Regex = {
		"[0-9][0-9](((([02468][048])|([13579][26]))\\-((((0[13578])|(1[02]))\\-((0[1-9])|([12][0-9])|(3[01])))|((02)\\-((0[1-9])|([1-2][0-9])))|(((0[469])|11)\\-((0[1-9])|([12][1-9])|(30)))))|((([02468][1235679])|([13579][01345789]))\\-((((0[13578])|(1[02]))\\-((0[1-9])|([12][0-9])|(3[01])))|((02)\\-((0[1-9])|(1[0-9])|(2[0-8])))|(((0[469])|([1][1]))\\-((0[1-9])|([12][0-9])|(30))))))".r
	}
	def evenParity : Regex = "(([02468]*[13579]){2})*[02468]*$".r
}