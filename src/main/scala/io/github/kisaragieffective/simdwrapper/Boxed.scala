package io.github.kisaragieffective.simdwrapper

// Type mapping from Scala primitives to Java boxed types
type Boxed[T] = T match {
  case Byte => java.lang.Byte
  case Short => java.lang.Short
  case Int => java.lang.Integer
  case Long => java.lang.Long
  case Float => java.lang.Float
  case Double => java.lang.Double
  case _ => T
}
