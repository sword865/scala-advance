package com.advance.fps.monad.free

trait Functor[F[_]] {
  def map[A,B](m: F[A])(f: A => B): F[B]
}

sealed trait Free[S[+_],+A] {

//  def flatMap[B](f: A => Free[S, A]): Free[S, A] = this match {
//    case FlatMap(a, g) =>
//      FlatMap(a, (x: Any) => g(x) flatMap f)
//    case x => FlatMap(x, f)
//  }
//
//  final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] =
//    this match {
//      case Done(a) => Right(a)
//      case More(k) => Left(k)
//      case a FlatMap f => a match {
//        case Done(a) => f(a).resume
//        case More(k) => Left(S.map(k)(_ flatMap f)) case b FlatMap g => b.flatMap((x: Any) => g(x) flatMap f).resume }
//    }
//
//  def zip[B](b: Free[S,B])(
//    implicit S: Functor[S]): Free[S, (A,B)] =
//    (resume , b.resume) match {
//      case (Left(a), Left(b)) => More(S.map(a)(x => More(S.map(b)(y => x zip y))))
//      case (Left(a), Right(b)) => More(S.map(a)(x => x zip Done(b)))
//      case (Right(a), Left(b)) => More(S.map(b)(y => Done(a) zip y))
//      case (Right(a), Right(b)) => Done((a, b))
//    }
}


private case class FlatMap[S[+_],A,+B](a: Free[S,A], f: A => Free[S,B]) extends Free[S,B]
case class Done[S[+_],+A](a: A) extends Free[S,A]
case class More[S[+_],+A](k: S[Free[S,A]]) extends Free[S,A]


object Free {
  type Trampoline[+A] = Free[Function0 , A]
  type Pair[+A] = (A, A)
  type BinTree[+A] = Free[Pair, A]
  type List[A] = Free[({type λ[+α] = (A, α)})#λ, Unit]

  def main(args: Array[String]): Unit = {

  }
}
