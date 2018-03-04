sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	// apply is a variadic function, which accepts zero or more arguments of type A
	// syntax: x : _* is used to convert x (a seq[A]) to a form to be passed to a variadic function
	def apply[A](as: A*): List[A] = 
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	// Ex2: tail - removing the first element of a list
	def tail[A](l: List[A]): List[A] = l match {
		case Nil => throw new IndexOutOfBoundsException("Nil list")
		case Cons(x, xs) => xs
	}

	// Ex3: drop - removing the first n elements of a list
	def drop[A](l: List[A], n: Int): List[A] = l match {
		case Nil => {
			if (n > 0) throw new IndexOutOfBoundsException("Nil list and n > 0")
			else Nil
		}
		case Cons(x, xs) => {
			if (n > 0) drop(xs, n - 1)
			else l
		}
	}

	// Ex4: dropWhile - removing elements from the List prefix as long as they match a predicate
	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
		case Nil => Nil 
		case Cons(x, xs) => {
			if (f(x)) dropWhile(xs)(f)
			else Cons(x, dropWhile(xs)(f))
		}
	}

	// Ex 5: setHead - replacing the first element of a List with a different value
	def setHead[A](l: List[A], h: A): List[A] = l match {
		case Nil => throw new NoSuchElementException("No head element found")
		case Cons(x, xs) => Cons(h, xs)
	}

	// Ex 6: init - return a List consisting of all but the last element of a List
	def init[A](l: List[A]): List[A] = l match {
		case Nil => throw new IndexOutOfBoundsException("Nil list")
		case Cons(x, Nil) => Nil
		case Cons(x, xs) => Cons(x, init(xs))
	}

	def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)
	
	def product2(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)
	
	// Ex 9: Compute the length of a list using foldRight
	def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => z + 1)

	// Ex 10: Implement foldLeft function
	def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
		case Nil => z 
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	// Ex 11: Implement sum, product and length using foldLeft
	def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
	
	def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

	def length2[A](l: List[A]): Int = foldLeft(l, 0)((z, _) => z + 1)

	// Ex 12: reverse - returns the reverse of a list
	def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

	// Ex 13: write foldLeft in terms of foldRight
	// and write foldRight in terms of foldLeft (this trick avoids stackoverflow)
	def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = 
		foldRight(reverse(l), z)((a, b) => f(b, a))

	def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
		foldLeft(reverse(l), z)((b, a) => f(a, b))

	// some functions for testing ex 13
	def sum4(l: List[Int]): Int = foldLeftViaFoldRight(l, 0)(_ + _)
	def product4(l: List[Double]): Double = foldRightViaFoldLeft(l, 1.0)(_ * _)	

	// Ex 14: implement append (in terms of foldLeft or foldRight)
	def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((h, acc) => Cons(h, acc))

	// Ex 15: implement concatenate 
	def concat[A](ls: List[List[A]]): List[A] = foldLeft(ls, Nil: List[A])(append)

	// Ex 16: transform a list of integer by adding 1 to each element
	def increment(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(x + 1, increment(xs))
	}

	// Ex 17: turns each value in a List[Double] into a String
	def doubleToString(l: List[Double]): List[String] = l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
	}

	// Ex 18: function map - generalizes increment and doubleToString
	def map[A, B](l: List[A])(f: A => B): List[B] = l match {
		case Nil => Nil 
		case Cons(x, xs) => Cons(f(x), map(xs)(f))
	}

	// Ex 19: filter function 
	def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => if (p(x)) Cons(x, filter(xs)(p)) else filter(xs)(p)
	}

	// Ex 20: flatMap function
	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

	// Ex 21: filter function (using flatMap function)
	def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] = 
		flatMap(l)(a => if(p(a)) List(a) else Nil)

	// Ex 22: contruct new list by adding corresponding elements from 2 input lists
	def addPairwise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
		case (Nil, _) => Nil 
		case (_, Nil) => Nil 
		case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
	}

	// Ex 23: generalization of ex 22
	def doPairwise[A, B](l: List[A], r: List[A])(f: (A, A) => B): List[B] = (l, r) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), doPairwise(xs, ys)(f))
	}

	// Ex 24: check if list l has subsequence sub 
	def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
		case (_, Nil) => true
		case (Nil, _) => false
		case (Cons(h, t), Cons(x, xs)) => {
			if (h != x) hasSubsequence(t, sub)
			else hasSubsequence(t, xs)
		}
	}
}