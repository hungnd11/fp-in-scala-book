type State[S, +A] = S => (A, S)
case class State[S, +A](run: S => (A, S))

sealed trait Input
case object Coin extends Input 
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
	def update = (i: Input) => (s: Machine) => {
		(i, s) match {
			case (_, Machine(_, 0, _)) => s 
			case (Coin, Machine(false, _, _)) => s
			case (Turn, Machine(true, _, _)) => s 
			case (Coin, Machine(true, candy, coin)) => 
				Machine(false, candy, coin + 1)
			case (Turn, Machine(false, candy, coin)) => 
				Machine(true, candy - 1, coin)
		}
	}

	def simulatedMachine(inputs: List[Input]): State[S, (Int, Int)] = inputs match {
		case List() => m => ((m.candies, m.coins), m)
		case h :: t => m => simulatedMachine(t)(update(h)(m))
	}
}