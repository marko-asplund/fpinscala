package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // e3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // e3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) =>  maximum(l) max maximum(r)
  }

  // e3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // e3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // e3.29
  def fold1[A, B](t: Tree[A])(acc: B)(f: (B, Tree[A]) => B): B = t match {
    case n: Leaf[A] => f(acc, n)
    case Branch(l, r) => f(fold1(r)(fold1(l)(acc)(f))(f), t)
  }
  def size2[A](t: Tree[A]): Int = fold1(t)(0)( (acc, n) => acc + 1 )

  def fold2[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold2(l)(f)(g), fold2(r)(f)(g))
  }
  def size3[A](t: Tree[A]): Int = fold2(t)(_ => 1)( (l, r) => 1 + l + r)
  def maximum2(t: Tree[Int]): Int = fold2(t)(v => v)( (l, r) => l max r)
  def depth2[A](t: Tree[A]): Int = fold2(t)(_ => 1)( (l, r) => (l max r) + 1)
  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold2(t)(v => Leaf(f(v)): Tree[B])( (l, r) => Branch(l, r) )

  def foo = {
    Branch(Branch(Leaf(1), Leaf(1)),Branch(Branch(Branch(Leaf(3), Leaf(-1)), Branch(Leaf(99), Leaf(11))), Leaf(11)))
  }

}