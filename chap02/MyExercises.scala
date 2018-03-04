object MyExercises {
	def fibonacci(n: Int): Int = {
		@annotation.tailrec
		def loop(n: Int, prev: Int, curr: Int): Int = {
			if (n <= 0) prev
			else if (n == 1) curr
			else loop(n - 1, curr, prev + curr)
		}

		loop(n, 0, 1)
	}

	def isSorted[T](array: Array[T], gt: (T, T) => Boolean): Boolean = {
		@annotation.tailrec
		def check(low: Int, high: Int): Boolean = 
			if (low == high) true
			else if (!gt(array(low), array(low + 1))) false
			else check(low + 1, high)

		check(0, array.length - 1)
	}

	def partial[A, B, C](a: A, f: (A, B) => C): B => C = 
		(b: B) => f(a, b)

	def curry[A, B, C](f: (A, B) => C): A => (B => C) =
		(a: A) => ((b: B) => f(a, b))

	def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = 
		(a: A, b: B) => f(a)(b)

	def compose[A, B, C](f: B => C, g: A => B): A => C = 
		(a: A) => f(g(a))

	def main(args: Array[String]): Unit = {
		println("Ex1: Fibonacci")
		println("The %d-th Fibonacci number: %d".format(10, fibonacci(10)))
		
		println("Ex2: isSorted")
		val ex20 = Array(1, 2, 3, 2)
		println("Test array: " + ex20.toList)
		println("Array is sorted? - Answer: " + isSorted(ex20, (x: Int, y: Int) => x < y))
		val ex21 = Array(1, 3, 5, 6, 7, 9)
		println("Test array: " + ex21.toList)
		println("Array is sorted? - Answer: " + isSorted(ex21, (x: Int, y: Int) => x < y))

		println("Ex3: A concrete usage of partial function")
		val p = partial(1, (x: Int, y: Double) => x.toString + y.toString)(1.1)
		println(p)

		println("Ex4: A concrete example of currying")
		val c = curry((x: Int, y: Double) => x.toString + y.toString)(1)(1.1)
		println(c)

		println("Ex5: A concrete example of uncurrying")
		val u = uncurry((x: Int) => (y: Double) => x.toString + y.toString)(1, 1.1)
		println(u)

		println("Ex6: A concrete example of composing")
		val o = compose((x: Double) => math.sin(x), (x: Double) => math.Pi / 2 - x)(math.Pi / 3)
		println(o)
	}
}