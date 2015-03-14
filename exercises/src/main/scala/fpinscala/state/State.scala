package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // e6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i: Int, r: RNG) if i < 0 => (-i, r)
    case (i: Int, r: RNG) if i == Int.MinValue => nonNegativeInt(r)
    case r => r
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // e6.2
  def double(rng: RNG): (Double, RNG) = {
    val r = nonNegativeInt(rng)
    (r._1.toDouble / Int.MaxValue.toDouble, r._2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val r1 = nonNegativeInt(rng)
    val r2 = double(r1._2)
    ((r1._1, r2._1), r2._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val r = intDouble(rng)
    (r._1.swap, r._2)
  }

  // e6.3
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val r1 = double(rng)
    val r2 = double(r1._2)
    val r3 = double(r2._2)
    ((r1._1, r2._1, r3._1), r3._2)
  }

  // e6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec def go(cnt: Int, rn: RNG, acc: IndexedSeq[Int]): (List[Int], RNG) = {
      if (cnt == 0) (acc.toList, rn)
      else {
        val newRn = rn.nextInt
        go(cnt - 1, newRn._2, acc :+ newRn._1)
      }
    }
    go(count, rng, IndexedSeq())
  }

  // e6.5
  def double2: Rand[Double] =
    map(int)(i => i.toDouble / Int.MaxValue.toDouble)

  // e6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rand => {
      val a = ra(rand)
      val b = rb(a._2)
      (f(a._1, b._1), b._2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // e6.7: impl. 1
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(l: List[Rand[A]], acc: List[A], rng: RNG): (List[A], RNG) = l match {
      case Nil => (acc, rng)
      case (h :: t) =>
        val r = h(rng)
        go(t, r._1 :: acc, r._2)
    }
    go(fs, List(), _)
  }

  // e6.7: impl. 2
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight((List[A]()), _) { (a, acc) =>
      val r = a(acc._2)
      (r._1 :: acc._1, r._2)
    }

  // e6.7: impl. 3
  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))( (a, b) => map2(a, b)(_ :: _))

  // e6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rand => {
      val a = f(rand)
      g(a._1)(a._2)
    }

  // e6.9
  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => (f(a), _))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap { r =>
      val a = ra(r)
      val b = rb(a._2)
      ((a._1, b._1), b._2)
    } ( ab =>
      (f(ab._1, ab._2), _)
    )

  def map2WithFlatMap2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) ( a => mapWithFlatMap(rb)(f(a, _)) )

}

case class State[S,+A](run: S => (A, S)) {
  // e6.10
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val e = run(s)
      (f(e._1), e._2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State( s => {
      val a = run(s)
      val b = sb.run(a._2)
      (f(a._1, b._1), b._2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State( s => {
      val a = run(s)
      f(a._1).run(a._2)
    })

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    State(s => {
      fs.foldRight(State.unit(List[A]()).run(s)) { (a, b) => {
        val n = a.run(b._2)
        (n._1 :: b._1, n._2)
      }
      }
    })
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  // e6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    @tailrec def go(m: Machine, cmds: List[Input]): Machine = {
      cmds match {
        case Coin :: t if m.locked && m.candies > 0 => go(m.copy(locked = false, coins = m.coins + 1), t)
        case Turn :: t if !m.locked && m.candies > 0 => go(m.copy(locked = true, candies = m.candies-1), t)
        case h :: t => go(m, t)
        case Nil => m
      }
    }
    State( s => {
      val m = go(s, inputs)
      ((m.coins, m.candies), m)
    })
  }

  // e6.10
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}
