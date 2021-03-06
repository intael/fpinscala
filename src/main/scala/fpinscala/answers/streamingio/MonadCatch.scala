package fpinscala.answers.streamingio

import fpinscala.answers.iomonad.*

import language.higherKinds

/*
 * A context in which exceptions can be caught and
 * thrown.
 */
trait MonadCatch[F[_]] extends Monad[F]:
  extension [A](fa: F[A])
    def attempt: F[Either[Throwable,A]]
  def fail[A](t: Throwable): F[A]

object MonadCatch:
  given taskInstance: MonadCatch[Task] with
    def unit[A](a: => A): Task[A] = Task(a)
    extension [A](fa: Task[A])
      def flatMap[B](f: A => Task[B]): Task[B] = Task.flatMap(fa)(f)
      def attempt: Task[Either[Throwable,A]] = Task.attempt(fa)
    def fail[A](err: Throwable): Task[A] = Task.fail(err)
