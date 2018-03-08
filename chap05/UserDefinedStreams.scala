object UserDefinedStream {
	def ones: Stream[Int] = 1 #:: ones

	def constant[A](a: A): Stream[A] = a #:: constant(a)

	def from(n: Int): Stream[Int] = n #:: from(n + 1)

	def fibs: Stream[Int] = {
		
		def loop(prev: Int, curr: Int): Stream[Int] = prev #:: loop(curr, prev + curr)

		loop(0, 1)
	}

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
		case Some((e, n)) => e #:: unfold(n)(f)
		case None => Stream.empty
	}

	def unfoldOnes: Stream[Int] = unfold(1)(Some(_, 1))
	def unfoldConstant[A](a: A): Stream[A] = unfold(a)(Some(_, a))
	def unfoldFrom(n: Int): Stream[Int] = unfold(n)(Some(_, n + 1))
	def unfoldFibs: Stream[Int] = unfold((0, 1))((s) => Some(s._1, (s._2, s._1 + s._2)))

	def startsWith[A](s: Stream[A], p: Stream[A]): Boolean = (s, p) match {
		case (_, Stream.Empty) => true
		case (hs #:: ts, hp #:: tp) => if (hs == hp) startsWith(ts, tp) else false
		case _ => false
	}

	def tails[A](s: Stream[A]): Stream[Stream[A]] = 
		Stream.Empty #:: unfold(s)(z => z match {
			case Stream.Empty => None
			case h #:: t => Some((z, t))
		})
}