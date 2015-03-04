package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // e5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n-1)) else Empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) t().drop(n-1) else Cons(h, t)
  }

  // e5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  // e5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (!p(h())) false else t().forAll(p)
  }

  // e5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])( (a, b) => if (p(a)) cons(a, b)  else empty )

  // e5.6
  def headOption: Option[A] = foldRight(None: Option[A])( (a, b) => Some(a))

  // e5.7
  def map[B](f: (A) => B): Stream[B] = foldRight(empty[B])( (a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])( (a, b) => if (p(a)) cons(a, b) else b )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)( (a, b) => cons(a, b) )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])( (a, b) => f(a) append b )

  // e5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // e5.13
  def map2[B](f: (A) => B): Stream[B] = unfold(this)(s => s match {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  })

  def take2(n: Int): Stream[A] = unfold((n, this))(s => s match {
    case (c, Cons(h, t)) if (c > 0) => Some(h(), (c-1, t()))
    case _ => None
  })
  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this)(s => s match {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  })
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2))(s => s match {
    case (Cons(s1h, s1t), Cons(s2h, s2t)) =>  Some(f(s1h(), s2h()), (s1t(), s2t()))
    case _ => None
  })
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2))(s => s match {
    case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some(((Some(s1h()), Some(s2h())), (s1t(), s2t())))
    case (Cons(s1h, s1t), _) => Some(((Some(s1h()), None), (s1t(), empty)))
    case (_, Cons(s2h, s2t)) => Some(((None, Some(s2h())), (empty, s2t())))
    case _ => None
  })

  // e5.14
  def startsWith[B](s: Stream[B]): Boolean = {
    !zipAll(s).exists(e => e match {
      case (Some(a), Some(b)) => a != b
      case (Some(a), None) => false
      case _ => true
    })
  }

  // e5.15
  def tails: Stream[Stream[A]] = unfold(this)(s => s match {
    case Empty => None
    case Cons(h, t) => Some(s, t())
  }).append(Stream(empty))

  // e5.16
  def scanRight[B](s: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(s)(f))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  // e5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // e5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // e5.10
  def fibs(n1: Int = 0, n2: Int = 1): Stream[Int] = Stream.cons(n1, fibs(n2, n1 + n2))

  // e5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    (f(z) map (r => cons(r._1, unfold(r._2)(f)))).getOrElse(empty)

  // e5.12
  def fibs2: Stream[Int] = unfold((0,1))( s => Some((s._1, (s._2, s._1 + s._2))))
  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))
  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))
  val ones2: Stream[Int] = constant2(1)

}