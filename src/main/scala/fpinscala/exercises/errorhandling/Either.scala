package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter

import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(value) => Right(f(value))
      case Left(value) => Left(value)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(_) => b
      case Right(value) => Right(value)


  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match
      case (Left(value), _) => Left(value)
      case (Right(value), Left(other)) => Left(other)
      case (Right(value), Right(other)) => Right(f(value, other))

object Either:

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = ???

  def traverseAll[E, A, B](es: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](es: List[Either[List[E], A]]): Either[List[E], List[A]] = ???
