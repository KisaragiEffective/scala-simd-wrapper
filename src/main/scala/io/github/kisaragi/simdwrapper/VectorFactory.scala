package io.github.kisaragi.simdwrapper

import jdk.incubator.vector._

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

// Type class for types that can be used to create vectors
trait VectorCreator[E] {
  type VectorType <: Vector[Boxed[E]]
  def fromArray(width: BitWidth, values: Array[E], offset: Int): VectorType
}

object VectorCreator {
  // Type class instances for supported types
  given VectorCreator[Byte] with {
    type VectorType = ByteVector
    def fromArray(width: BitWidth, values: Array[Byte], offset: Int): ByteVector = {
      val species = width match {
        case BitWidth.Maximum =>   ByteVector.SPECIES_MAX
        case BitWidth.Of128 =>     ByteVector.SPECIES_128
        case BitWidth.Of256 =>     ByteVector.SPECIES_256
        case BitWidth.Of512 =>     ByteVector.SPECIES_512
        case BitWidth.Of64 =>      ByteVector.SPECIES_64
        case BitWidth.Preferred => ByteVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsBytes()
    }
  }

  given VectorCreator[Short] with {
    type VectorType = ShortVector
    def fromArray(width: BitWidth, values: Array[Short], offset: Int): ShortVector = {
      val species = width match {
        case BitWidth.Maximum =>   ShortVector.SPECIES_MAX
        case BitWidth.Of128 =>     ShortVector.SPECIES_128
        case BitWidth.Of256 =>     ShortVector.SPECIES_256
        case BitWidth.Of512 =>     ShortVector.SPECIES_512
        case BitWidth.Of64 =>      ShortVector.SPECIES_64
        case BitWidth.Preferred => ShortVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsShorts()
    }
  }

  given VectorCreator[Int] with {
    type VectorType = IntVector
    def fromArray(width: BitWidth, values: Array[Int], offset: Int): IntVector = {
      val species = width match {
        case BitWidth.Maximum =>   IntVector.SPECIES_MAX
        case BitWidth.Of128 =>     IntVector.SPECIES_128
        case BitWidth.Of256 =>     IntVector.SPECIES_256
        case BitWidth.Of512 =>     IntVector.SPECIES_512
        case BitWidth.Of64 =>      IntVector.SPECIES_64
        case BitWidth.Preferred => IntVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsInts()
    }
  }

  given VectorCreator[Long] with {
    type VectorType = LongVector
    def fromArray(width: BitWidth, values: Array[Long], offset: Int): LongVector = {
      val species = width match {
        case BitWidth.Maximum =>   LongVector.SPECIES_MAX
        case BitWidth.Of128 =>     LongVector.SPECIES_128
        case BitWidth.Of256 =>     LongVector.SPECIES_256
        case BitWidth.Of512 =>     LongVector.SPECIES_512
        case BitWidth.Of64 =>      LongVector.SPECIES_64
        case BitWidth.Preferred => LongVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsLongs()
    }
  }

  given VectorCreator[Float] with {
    type VectorType = FloatVector
    def fromArray(width: BitWidth, values: Array[Float], offset: Int): FloatVector = {
      val species = width match {
        case BitWidth.Maximum =>   FloatVector.SPECIES_MAX
        case BitWidth.Of128 =>     FloatVector.SPECIES_128
        case BitWidth.Of256 =>     FloatVector.SPECIES_256
        case BitWidth.Of512 =>     FloatVector.SPECIES_512
        case BitWidth.Of64 =>      FloatVector.SPECIES_64
        case BitWidth.Preferred => FloatVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsFloats()
    }
  }

  given VectorCreator[Double] with {
    type VectorType = DoubleVector
    def fromArray(width: BitWidth, values: Array[Double], offset: Int): DoubleVector = {
      val species = width match {
        case BitWidth.Maximum =>   DoubleVector.SPECIES_MAX
        case BitWidth.Of128 =>     DoubleVector.SPECIES_128
        case BitWidth.Of256 =>     DoubleVector.SPECIES_256
        case BitWidth.Of512 =>     DoubleVector.SPECIES_512
        case BitWidth.Of64 =>      DoubleVector.SPECIES_64
        case BitWidth.Preferred => DoubleVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsDoubles()
    }
  }
}

object VectorFactory {
  // Type-safe factory method using type class
  def apply[E](width: BitWidth, values: Array[E], offset: Int)(using creator: VectorCreator[E]): creator.VectorType =
    creator.fromArray(width, values, offset)

  /*
  NOTE: reinterpretAsXxx is totally safe, is O(1), and is necessary because `species.fromArray`
  doesn't return unboxed version of Vector on signature.
  However, it `return`s `this` from unboxed versions.
  
  The old overloaded methods have been replaced with a type class approach for type safety.
  */

  def filled(width: BitWidth, value: Byte): ByteVector = {
    val species = width match {
      case BitWidth.Maximum =>   ByteVector.SPECIES_MAX
      case BitWidth.Of128 =>     ByteVector.SPECIES_128
      case BitWidth.Of256 =>     ByteVector.SPECIES_256
      case BitWidth.Of512 =>     ByteVector.SPECIES_512
      case BitWidth.Of64 =>      ByteVector.SPECIES_64
      case BitWidth.Preferred => ByteVector.SPECIES_PREFERRED
    }
    ByteVector.broadcast(species, value)
  }
  // TODO: fixed arity factory
  // TODO: filled
  // TODO: zero
}
