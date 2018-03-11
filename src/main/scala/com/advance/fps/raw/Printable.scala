package com.advance.fps.raw

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val intPrintable: Printable[Int] {
  } = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val stringPrintable: Printable[String] {
  } = new Printable[String] {
    override def format(value: String): String = value
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))

  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    print(1)
  }
}
