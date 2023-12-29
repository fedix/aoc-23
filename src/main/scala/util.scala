package util

extension [A](a: A)
  inline infix def |>[B](f: A => B): B =
    f(a)
