import java.util.regex._

object PatternMatching {
	// examples of lifting functions
	def pattern(s: String): Option[Pattern] = 
		try {
			Some(Pattern.compile(s))
		} catch {
			case e: PatternSyntaxException => None
		}

	def makeMatcher(pat: String): Option[String => Boolean] = 
		pattern(pat) map (p => (s: String) => p.matcher(s).matches)

	// using for-comprehension to lift a function
	def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] = 
		for {
			f <- makeMatcher(pat1)
			g <- makeMatcher(pat2)
		} yield f(s) && g(s)
	
	// Ex 4: bothMatch_2 in term of map2
	def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
		a.flatMap(av => (b.map(bv => f(av, bv))))

	def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = 
		map2(makeMatcher(pat1), makeMatcher(pat2))((f, g) => f(s) && g(s))
	
	def main(args: Array[String]): Unit = {
		val pat1 = "[a-zA-Z]"
		val pat2 = "[^0-9]"

		println(makeMatcher(pat1).map(m => m("m")).getOrElse(false))
		println(bothMatch(pat1, pat2, "m").getOrElse(false))
		println(bothMatch_2(pat1, pat2, "m").getOrElse(false))
	}
}