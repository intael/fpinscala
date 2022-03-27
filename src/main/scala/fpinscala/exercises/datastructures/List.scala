package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Attempted to tail an empty list")
      case Cons(_, rest) => rest
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("Attempted to set head on empty list")
      case Cons(_, rest) => Cons(h, rest)
    }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    (l, n) match {
      case (Cons(_, rest), _) if n > 0 => drop(rest, n - 1)
      case (Nil, _) => Nil
      case _ => l
    }


  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, _) if f(head) => dropWhile(tail(l), f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (ls: A, count: Int) => count + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match {
      case Nil => acc
      case Cons(head, rest) => foldLeft(rest, f(acc, head), f)
    }

  def sumViaFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0, (accumulator, value) => accumulator + value)

  def productViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0, (accumulator, value) => accumulator * value)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (accumulator, _) => accumulator + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil, (accumulator: List[A], value: A) => Cons(value, accumulator))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (left, right) => Cons(left, right))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List(), (accumulator: List[A], value: List[A]) => appendViaFoldRight(accumulator, value))

  def incrementEach(l: List[Int]): List[Int] =
    l match {
      case Cons(head, rest) => Cons(head + 1, incrementEach(rest))
      case _ => Nil
    }

  def doubleToString(l: List[Double]): List[String] =
    l match {
      case Cons(head, rest) => Cons(head.toString, doubleToString(rest))
      case _ => Nil
    }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Cons(head, rest) => Cons(f(head), map(rest)(f))
      case _ => Nil
    }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
      case Cons(head, tail) => filter(tail)(f)
      case _ => Nil
    }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Cons(head, tail) => append(f(head), flatMap(tail)(f))
      case _ => Nil
    }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)( value => if(f(value)) List(value) else Nil )

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    (a, b) match {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons(aHead + bHead, addPairwise(aTail, bTail))
      case _ => Nil
    }
  }

  // def zipWith - TODO determine signature
  def zipWith[A](a: List[A], b: List[A], combine: (A, A) => A): List[A] =
    (a, b) match {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons(combine(aHead, bHead), zipWith(aTail, bTail, combine))
      case _ => Nil
    }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
