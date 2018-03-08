// avoid conflicts against the std lib
import scala.{Option => _, Either => _, None => _, Some => _}

sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(v) => Some(f(v))
	}
	
	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(v) => v
	}

	def filter(f: A => Boolean): Option[A] = this match {
		case None => None 
		case Some(v) => if (f(v)) this else None
	}

	// Implement flatMap, orElse with map and getOrElse
	// flatMap = map then flatten
	def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

	// orElse return the first Option if it is defined, otherwise return the second option
	// similar to getOrElse, except that we return another Option if the first is undefined
	def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

	// Implement flatMap, orElse without map and getOrElse
	def flatMap1[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(v) => f(v)
	}

	def orElse1[B >: A](ob: => Option[B]): Option[B] = this match {
		case None => ob 
		case Some(v) => this
	}
	
	// It is possible to implement filter in term of flatMap 
	def filter1(f: A => Boolean): Option[A] = this.flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
	def mean(xs: Seq[Double]): Option[Double] = 
		if (xs.isEmpty) None 
		else Some(xs.sum / xs.length)

	// Ex 2: Implement the variance function
	def variance(xs: Seq[Double]): Option[Double] = 
		mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

	// using common idiom o.getOrElse(throw new Exception("FAIL!")) to throw exception
	def varianceInValue(xs: Seq[Double]): Double = 
		variance(xs).getOrElse(throw new Exception("Empty sequence!"))

	// Ex 3: map2 - combines 2 Option values using a binary function
	def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
		a.flatMap(av => (b.map(bv => f(av, bv))))

	// Ex 5: combine a list of Options into one Option
	// containing a list of all the Some values in the original list
	def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs match {
		case List() => Some(List())
		case h :: t => h.flatMap(hv => sequence(t).map(hv :: _))
	}

	// Ex 6: implement traverse
	def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = xs match {
		case List() => Some(List())
		case h :: t => f(h).flatMap(fhv => traverse(t)(f).map(fhv :: _))
	}

	// Using the above implementation, if we implement sequence in term of traverse
	// we need a function f such that f(h) = h, so the function is x => x
	def traverseSequence[A](xs: List[Option[A]]): Option[List[A]] = 
		traverse(xs)(x => x)
}