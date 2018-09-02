package com.advance.fps.monad.trampoline

sealed trait Trampoline[+A] {

  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k().runT
  }

  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(k) => Left(k)
    case FlatMap(a, f) => a match {
      case Done(v) => f(v).resume
      case More(k) => Left(() =>
        FlatMap(k(), f))
      case FlatMap(b,g) => b.flatMap((x:Any) => g(x) flatMap f).resume
//      case FlatMap(b,g) => (FlatMap(b, (x:Any) => FlatMap(g(x), f)):Trampoline[A]).resume
    }}

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case FlatMap(a, g) =>
      FlatMap(a, (x: Any) => g(x) flatMap f)
    case x => FlatMap(x, f)
  }

  def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))

  def zip[B](b: Trampoline[B]): Trampoline[(A,B)] = (this.resume , b.resume) match {
    case (Right(a), Right(b)) => Done((a, b))
    case (Left(a), Left(b)) => More(() => a() zip b())
    case (Left(a), Right(b)) => More(() => a() zip Done(b))
    case (Right(a), Left(b)) => More(() => Done(a) zip b())
  }
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A) extends Trampoline[A]
private case class FlatMap[A,+B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

case class State[S,+A](runS: S => Trampoline[(A,S)]){
  def flatMap[B](f: A => State[S,B]): State[S, B] = State[S,B](s =>
    More(
      () => runS(s) flatMap {
        case (a,s1) => More(() => f(a) runS s1)
  }))
}

object Trampoline {
  implicit def step[A](a: => A): Trampoline[A] = More(() => Done(a))

  def main(args: Array[String]): Unit = {
    val result = for {
      x <- 1 + 1
      y <- x + 2
      z <- y + 3
    } yield z
    println(result.runT)

    val hello: Trampoline[Unit] = for {
      _ <- print("Hello ,␣")
      _ <- println("World!")
    } yield ()

    (hello zip hello).runT
  }
}

