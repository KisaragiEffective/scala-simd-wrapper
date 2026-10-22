package io.github.kisaragieffective.simdwrapper

import jdk.incubator.vector._

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
      summon[VectorSpeciesCreator[Byte]].create(width).fromArray(values, offset).reinterpretAsBytes()
    }
  }

  given VectorCreator[Short] with {
    type VectorType = ShortVector
    def fromArray(width: BitWidth, values: Array[Short], offset: Int): ShortVector = {
      summon[VectorSpeciesCreator[Short]].create(width).fromArray(values, offset).reinterpretAsShorts()
    }
  }

  given VectorCreator[Int] with {
    type VectorType = IntVector
    def fromArray(width: BitWidth, values: Array[Int], offset: Int): IntVector = {
      summon[VectorSpeciesCreator[Int]].create(width).fromArray(values, offset).reinterpretAsInts()
    }
  }

  given VectorCreator[Long] with {
    type VectorType = LongVector
    def fromArray(width: BitWidth, values: Array[Long], offset: Int): LongVector = {
      summon[VectorSpeciesCreator[Long]].create(width).fromArray(values, offset).reinterpretAsLongs()
    }
  }

  given VectorCreator[Float] with {
    type VectorType = FloatVector
    def fromArray(width: BitWidth, values: Array[Float], offset: Int): FloatVector = {
      summon[VectorSpeciesCreator[Float]].create(width).fromArray(values, offset).reinterpretAsFloats()
    }
  }

  given VectorCreator[Double] with {
    type VectorType = DoubleVector
    def fromArray(width: BitWidth, values: Array[Double], offset: Int): DoubleVector = {
      summon[VectorSpeciesCreator[Double]].create(width).fromArray(values, offset).reinterpretAsDoubles()
    }
  }
}
