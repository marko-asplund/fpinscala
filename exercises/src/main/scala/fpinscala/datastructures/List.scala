package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // e3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // e3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  // e3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(x, xs) if (n > 0) => drop(xs, n-1)
    case _ => l
  }

  // e3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }

  // e3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // e3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  // e3.10
  @tailrec def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // e3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  // e3.12
  def reverse[A](l: List[A]) = foldRight(l, List[A]())((a, b) => append(b, Cons(a, Nil)))
  def reverse2[A](l: List[A]) = foldLeft(l, List[A]())((a, b) => Cons(b, a))

  // e3.13
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse2(l), z)( (a, b) => f(b, a))
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)( (b, a) => f(a, b))

  // e3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse2(a1), a2)((a, b) => Cons(b, a))

  // e3.15
  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())((a, b) => append2(a, b))

  // e3.16
  def addOne(l: List[Int]) = foldRight(l, List[Int]())((a, b) => Cons(a+1, b))

  // e3.17
  def toString(l: List[Double]) = foldRight(l, List[String]())((a, b) => Cons(a.toString, b))

  // e3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldLeft(reverse2(l), List[B]())((b, a) => Cons(f(a), b))

  // e3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  // e3.20:
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldLeft(as, List[B]())((b, a) => append(b, f(a)))

  // e3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(i => if(f(i)) List(i) else Nil)

  // e3.22
  def elementWiseSum(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(l1h, l1t) => l2 match {
      case Cons(l2h, l2t) => Cons(l1h+l2h, elementWiseSum(l1t, l2t))
      case _ => require(false, "lists must of of same length"); Nil
    }
  }

  // e3.23
  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = l1 match {
    case Nil => Nil
    case Cons(l1h, l1t) => l2 match {
      case Cons(l2h, l2t) => Cons(f(l1h, l2h), zipWith(l1t, l2t)(f))
      case _ => require(false, "lists must of of same length"); Nil
    }
  }

  // e3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(l: List[A], prefix: List[A]): Boolean = prefix match {
      case Nil => true
      case Cons(ph, pt) => l match {
        case Cons(lh, lt) => if ( ph == lh ) startsWith(lt, pt) else false
        case _ => false
      }
    }
    sup match {
      case Nil => false
      case Cons(h, t) => if (startsWith(Cons(h, t), sub)) true else hasSubsequence(t, sub)
    }
  }
}