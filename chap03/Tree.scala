sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
	// Ex 25: size - counts the number of nodes in a tree
	def size[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(l) + size(r)
	}

	// Ex 26: maximun - return the maximun element in a Tree[Int]
	def maximum(tree: Tree[Int]): Int = tree match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max (maximum(r))
	}

	// Ex 27: depth - maximum path length from the root of a tree to any leaf 
	def depth[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 0
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

	// Ex 28: map tree to tree 
	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
		case Leaf(v) =>  Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	// Ex 29: fold - generalization of size, maximum, depth and map 
	// f: function maps each value in each leaf to a new value in type B 
	// g: function combines 2 branches of a tree
	def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
		case Leaf(v) => f(v)
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}
	
	// Reimplement size, maximum, depth and map in terms of function fold
	def foldSize[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
	
	def foldMaximum(tree: Tree[Int]): Int = fold(tree)(v => v)(_ max _)
	
	def foldDepth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((d1, d2) => 1 + (d1 max d2))

	def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = 
		fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}