package com.advance.fps.raw

trait Printable[A] {
  def format(value: A): String
}

final case class Cat(name: String, age: Int, color: String)

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)
    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }
}

object PrintableInstances {
  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString

  implicit val stringPrintable: Printable[String] = (value: String) => value

  implicit val catPrintable: Printable[Cat] = (cat: Cat) => f"${cat.name} is a ${cat.age} year-old ${cat.color} cat"

}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))

  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    import PrintableSyntax._
    print(Cat("kitty", 10, "white"))
    Cat("kitty", 10, "white").print
  }
}
