trait RNG {
	def nextInt: (Int, RNG) 
}

type State[S, +A] = S => (A, S)
case class State[S, +A](run: S => (A, S))
type Rand[+A] = State[RNG, A]

object RNG {
	def simple(seed: Long): RNG = new RNG {
		def nextInt = {
			val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
			((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
		}
	}

	def randomPair(rng: RNG): ((Int, Int), RNG) = {
		val (i1, rng1) = rng.nextInt
		val (i2, rng2) = rng1.nextInt
		((i1, i2), rng2)
	}

	// Ex 1: next positive integer number
	def positive(rng: RNG): (Int, RNG) = {
		val (n, r) = rng.nextInt
		if (n == Int.MinValue) throw new ArithmeticException("Int.MinValue.abs is not existed")
		else (n.abs, r)
	}

	// Ex 2: generate a Double between 0 and 1
	def double(rng: RNG): (Double, RNG) = {
		val (x, r) = rng.nextInt
		(x.toDouble / Int.MaxValue, r)
	}

	// Ex 3: intDouble, doubleInt, double3
	def intDouble(rng: RNG): ((Int, Double), RNG) = {
		val (n, r) = randomPair(rng)
		((n._1, n._2.toDouble / Int.MaxValue), r)
	}

	def doubleInt(rng: RNG): ((Double, Int), RNG) = {
		val (n, r) = randomPair(rng)
		((n._1.toDouble / Int.MaxValue, n._2), r)
	}

	def double3(rng: RNG): ((Double, Double, Double), RNG) = {
		val (d1, r1) = double(rng)
		val (d2, r2) = double(r1)
		val (d3, r3) = double(r2)
		((d1, d2, d3), r3)
	}

	// Ex 4: Write a function to generate a list of random integers
	def ints(counts: Int)(rng: RNG): (List[Int], RNG) = {
		if (counts > 0) {
			val (n, r) = rng.nextInt
			(n :: ints(counts - 1)(r)._1, r)
		}
		else (List(), rng)
	}
	
	// Using generalized API to generate random number
	val int: Rand[Int] = _.nextInt

	def unit[A](a: A): Rand[A] = rng => (a, rng)

	def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
		val (a, rng1) = s(rng)
		(f(a), rng1)
	}

	// Ex 5: use map to generate an Int between 0 and n, inclusive
	def positiveMax(n: Int): Rand[Int] = map(int)(v => v % n)

	// Ex 6: use map to reimplement RNG.double 
	def doubleViaMap: Rand[Double] = map(int)(v => v.toDouble / Int.MaxValue)

	// Ex 7: map2 combinator function
	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
		val (a, rng1) = ra(rng)
		val (b, rng2) = rb(rng1)
		(f(a, b), rng2)
	}

	// Re-implement the intDouble and doubleInt functions
	def intDoubleViaMap2: Rand[(Int, Double)] = 
		map2(int, doubleViaMap)((i, d) => (i, d))

	def doubleIntViaMap2: Rand[(Double, Int)] = 
		map2(doubleViaMap, int)((d, i) => (d, i))

	// Ex 8: combine a list of transitions into a single transition 
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
		case List() => (List(), _)
		case fh :: ft => rng => {
			val (a, r1) = fh(rng)
			val (t, r2) = sequence(ft)(r1)
			(a :: t, r2)
		}
	}

	def intsViaSequence(counts: Int): Rand[List[Int]] = 
		sequence(List.fill(counts)(int))

	// Ex 9: flatMap function
	def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
		val (a, r) = f(rng)
		g(a)(r)
	}

	// Ex 10: reimplement map and map2 using flatMap function
	def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = 
		flatMap(s)(a => (f(a), _))

	def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
		flatMap(ra)(a => flatMap(rb)(b => (f(a, b), _)))
}