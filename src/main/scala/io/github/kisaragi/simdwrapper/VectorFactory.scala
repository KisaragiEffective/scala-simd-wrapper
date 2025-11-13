package io.github.kisaragi.simdwrapper

import jdk.incubator.vector._

// Type class for types that can be used to create vectors
trait VectorCreator[E] {
  def fromArray(width: BitWidth, values: Array[E], offset: Int): Vector[E]
}

object VectorCreator {
  // Type class instances for supported types
  given VectorCreator[Byte] with {
    def fromArray(width: BitWidth, values: Array[Byte], offset: Int): Vector[Byte] = {
      val species = width match {
        case BitWidth.Maximum =>   ByteVector.SPECIES_MAX
        case BitWidth.Of128 =>     ByteVector.SPECIES_128
        case BitWidth.Of256 =>     ByteVector.SPECIES_256
        case BitWidth.Of512 =>     ByteVector.SPECIES_512
        case BitWidth.Of64 =>      ByteVector.SPECIES_64
        case BitWidth.Preferred => ByteVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsBytes().asInstanceOf[Vector[Byte]]
    }
  }

  given VectorCreator[Short] with {
    def fromArray(width: BitWidth, values: Array[Short], offset: Int): Vector[Short] = {
      val species = width match {
        case BitWidth.Maximum =>   ShortVector.SPECIES_MAX
        case BitWidth.Of128 =>     ShortVector.SPECIES_128
        case BitWidth.Of256 =>     ShortVector.SPECIES_256
        case BitWidth.Of512 =>     ShortVector.SPECIES_512
        case BitWidth.Of64 =>      ShortVector.SPECIES_64
        case BitWidth.Preferred => ShortVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsShorts().asInstanceOf[Vector[Short]]
    }
  }

  given VectorCreator[Int] with {
    def fromArray(width: BitWidth, values: Array[Int], offset: Int): Vector[Int] = {
      val species = width match {
        case BitWidth.Maximum =>   IntVector.SPECIES_MAX
        case BitWidth.Of128 =>     IntVector.SPECIES_128
        case BitWidth.Of256 =>     IntVector.SPECIES_256
        case BitWidth.Of512 =>     IntVector.SPECIES_512
        case BitWidth.Of64 =>      IntVector.SPECIES_64
        case BitWidth.Preferred => IntVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsInts().asInstanceOf[Vector[Int]]
    }
  }

  given VectorCreator[Long] with {
    def fromArray(width: BitWidth, values: Array[Long], offset: Int): Vector[Long] = {
      val species = width match {
        case BitWidth.Maximum =>   LongVector.SPECIES_MAX
        case BitWidth.Of128 =>     LongVector.SPECIES_128
        case BitWidth.Of256 =>     LongVector.SPECIES_256
        case BitWidth.Of512 =>     LongVector.SPECIES_512
        case BitWidth.Of64 =>      LongVector.SPECIES_64
        case BitWidth.Preferred => LongVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsLongs().asInstanceOf[Vector[Long]]
    }
  }

  given VectorCreator[Float] with {
    def fromArray(width: BitWidth, values: Array[Float], offset: Int): Vector[Float] = {
      val species = width match {
        case BitWidth.Maximum =>   FloatVector.SPECIES_MAX
        case BitWidth.Of128 =>     FloatVector.SPECIES_128
        case BitWidth.Of256 =>     FloatVector.SPECIES_256
        case BitWidth.Of512 =>     FloatVector.SPECIES_512
        case BitWidth.Of64 =>      FloatVector.SPECIES_64
        case BitWidth.Preferred => FloatVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsFloats().asInstanceOf[Vector[Float]]
    }
  }

  given VectorCreator[Double] with {
    def fromArray(width: BitWidth, values: Array[Double], offset: Int): Vector[Double] = {
      val species = width match {
        case BitWidth.Maximum =>   DoubleVector.SPECIES_MAX
        case BitWidth.Of128 =>     DoubleVector.SPECIES_128
        case BitWidth.Of256 =>     DoubleVector.SPECIES_256
        case BitWidth.Of512 =>     DoubleVector.SPECIES_512
        case BitWidth.Of64 =>      DoubleVector.SPECIES_64
        case BitWidth.Preferred => DoubleVector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAsDoubles().asInstanceOf[Vector[Double]]
    }
  }
}

object VectorFactory {
  // Type-safe factory method using type class
  def apply[E](width: BitWidth, values: Array[E], offset: Int)(using creator: VectorCreator[E]): Vector[E] =
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
