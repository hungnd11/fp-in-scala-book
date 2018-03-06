import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
	// Ex 7: Implement map, flatMap, orElse, map2
	def map[B](f: A => B): Either[E, B] = this match {
		case Left(v) => Left(v)
		case Right(v) => Right(f(v))
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Left(v) => Left(v)
		case Right(v) => f(v)
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
		case Left(v) => b
		case Right(v) => Right(v)
	}

	// Should read the for-comprehension paragraph right after the exercise
	// It will result in Left(_) if any expression is Left(_)
	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
		for {
			va <- this
			vb <- b
		} yield f(va, vb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
	def mean(xs: Seq[Double]): Either[String, Double] = 
		if (xs.isEmpty) Left("mean of empty list!")
		else Right(xs.sum / xs.length)

	def safeDiv(x: Double, y: Double): Either[Exception, Double] = 
		try {
			Right(x / y)
		} catch {
			case e: Exception => Left(e)
		}

	// Ex 8: Implement sequence and traverse
	def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = xs match {
		case List() => Right(List())
		case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
	}

	def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] = xs match {
		case List() => Right(List())
		case h :: t => f(h).flatMap(fhh => traverse(t)(f).map(fhh :: _))
	}

	def traverseSequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = 
		traverse(xs)(x => x)
}