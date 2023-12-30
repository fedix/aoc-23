package util

import cats.derived.*

import scala.util.Using
import scala.io.Source

type Result[A] = Either[String, A]

extension [A](a: A)
  inline infix def |>[B](f: A => B): B =
    f(a)

// doesn't work as expected yet
extension (obj: Ordering.type)
  inline def derived[A]: Ordering[A] =
    DerivedOrder[A].toOrdering

def inputLines(path: String): List[String] =
  Using(Source.fromResource(path))(_.getLines().toList).get
