object PathImplicits {
		

	import java.nio.file._

	implicit class RichString(s: String){
		def /(s2: String) : Path = Paths.get(s).resolve(s2)
		def /(p2: Path) : Path = Paths.get(s).resolve(p2)
	}

	implicit class RichPath (p: Path){
		def /(s2: String) : Path = p.resolve(s2)
		def /(p2: Path) : Path = p.resolve(p2)
		def write(str: String): Path = Files.write(p,str.getBytes)
		def read() : String = new String(Files.readAllBytes(p))
		def append(str: String): Path = {
			if(!Files.exists(p)){
				write("")
			}
			val s = read().concat(str)
			write(s)
		}
	}

}
