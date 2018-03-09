type State[S, +A] = S => (A, S)
case class State[S, +A](run: S => (A, S))

object State {
	// Ex 11: generalize unit, map, map2, flatMap and sequence
	def unit[S, A](a: A): State[S, A] = s => (a, s)

	def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = s0 => {
		val (a, s1) = s(e)
		(f(a), s1)
	}

	def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = s0 => {
		val (a, s1) = sa(s0)
		val (b, s2) = sb(s1)
		(f(a, b), s2)
	}

	def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = s0 => {
		val (a, s1) = f(s0)
		g(a)(s1)
	}

	def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
		case List() => (List(), _)
		case fh :: ft => s0 => {
			val (a, s1) = fh(s0)
			val (t, s2) = sequence(ft)(s1)
			(a :: t, s2)
		}
	}

	// Ex 11: get and set
	def get[S]: State[S, S] = State(s => (s, s))

	def set[S](s: S): State[S, Unit] = State(_ => ((), s))

	def modify[S](f: S => S): State[S, Unit] = for {
		s <- get
		_ <- set(f(s))
	} yield () 
}