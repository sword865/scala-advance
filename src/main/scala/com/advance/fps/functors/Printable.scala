package com.advance.fps.functors

import com.advance.fps.intro.Printable

trait Printable[A] {
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    (value: B) => Printable.this.format(func(value))
}

object Printable {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]) = p.contramap[Box[A]](_.value)


  def main(args: Array[String]): Unit = {
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    println(format("Hello"))
    println(format(true))
    println(format(Box("hello world")))
  }
}
