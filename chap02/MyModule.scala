object MyModule {
	def abs(n: Int): Int = 
		if (n < 0) -n else n

	private def formatAbs(x: Int): String = {
		val msg = "The absolute value of %d is %d."
		msg.format(x, abs(x))
	}

	def factorial(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, acc: Int): Int = {
			if (n <= 0) acc
			else go(n - 1, acc * n)
		}

		go(n, 1)
	}

	def formatResult(name: String, n: Int, f: Int => Int): String = {
		val msg = "The %s of %d is %d."
		msg.format(name, n, f(n))
	}

	def binarySearch[T](as: Array[T], key: T, gt: (T, T) => Boolean): Int = {
		@annotation.tailrec
		def go(low: Int, mid: Int, high: Int): Int = {
			if (low > high) -mid - 1
			else {
				val mid2 = (low + high) / 2
				val a = as(mid2)
				val greater = gt(a, key)
				if (!greater && !gt(key, a)) mid2
				else if (greater) go(low, mid2, mid2 - 1)
				else go(mid2 + 1, mid2, high)
			}
		}

		go(0, 0, as.length - 1)
	}

	def main(arg: Array[String]): Unit = {
		println(formatResult("absolute value", -5, abs))
		println(formatResult("factorial", 5, factorial))
	}
}