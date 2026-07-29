package io.github.kisaragieffective.simdwrapper

import jdk.incubator.vector.ByteVector

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
