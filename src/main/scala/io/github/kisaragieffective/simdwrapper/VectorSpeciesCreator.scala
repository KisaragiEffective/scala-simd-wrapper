package io.github.kisaragieffective.simdwrapper

import jdk.incubator.vector._

trait VectorSpeciesCreator[T] {
  type SpeciesType <: VectorSpecies[Boxed[T]]

  def create(bitWidth: BitWidth): SpeciesType
}

object VectorSpeciesCreator {
  given VectorSpeciesCreator[Byte] with {
    override type SpeciesType = VectorSpecies[Boxed[Byte]]

    override def create(bitWidth: BitWidth): SpeciesType = width match {
      case BitWidth.Maximum =>    ByteVector.SPECIES_MAX
      case BitWidth.Of128 =>      ByteVector.SPECIES_128
      case BitWidth.Of256 =>      ByteVector.SPECIES_256
      case BitWidth.Of512 =>      ByteVector.SPECIES_512
      case BitWidth.Of64 =>       ByteVector.SPECIES_64
      case BitWidth.Preferred =>  ByteVector.SPECIES_PREFERRED
    }
  }

  given VectorSpeciesCreator[Short] with {
    override type SpeciesType = VectorSpecies[Boxed[Short]]

    override def create(bitWidth: BitWidth): SpeciesType = width match {
      case BitWidth.Maximum =>    ShortVector.SPECIES_MAX
      case BitWidth.Of128 =>      ShortVector.SPECIES_128
      case BitWidth.Of256 =>      ShortVector.SPECIES_256
      case BitWidth.Of512 =>      ShortVector.SPECIES_512
      case BitWidth.Of64 =>       ShortVector.SPECIES_64
      case BitWidth.Preferred =>  ShortVector.SPECIES_PREFERRED
    }
  }

  given VectorSpeciesCreator[Int] with {
    override type SpeciesType = VectorSpecies[Boxed[Int]]

    override def create(bitWidth: BitWidth): SpeciesType = width match {
      case BitWidth.Maximum =>    IntVector.SPECIES_MAX
      case BitWidth.Of128 =>      IntVector.SPECIES_128
      case BitWidth.Of256 =>      IntVector.SPECIES_256
      case BitWidth.Of512 =>      IntVector.SPECIES_512
      case BitWidth.Of64 =>       IntVector.SPECIES_64
      case BitWidth.Preferred =>  IntVector.SPECIES_PREFERRED
    }
  }

  given VectorSpeciesCreator[Long] with {
    override type SpeciesType = VectorSpecies[Boxed[Long]]

    override def create(bitWidth: BitWidth): SpeciesType = width match {
      case BitWidth.Maximum =>    LongVector.SPECIES_MAX
      case BitWidth.Of128 =>      LongVector.SPECIES_128
      case BitWidth.Of256 =>      LongVector.SPECIES_256
      case BitWidth.Of512 =>      LongVector.SPECIES_512
      case BitWidth.Of64 =>       LongVector.SPECIES_64
      case BitWidth.Preferred =>  LongVector.SPECIES_PREFERRED
    }
  }
  
  given VectorSpeciesCreator[Float] with {
    override type SpeciesType = VectorSpecies[Boxed[Float]]

    override def create(bitWidth: BitWidth): SpeciesType = width match {
      case BitWidth.Maximum =>    FloatVector.SPECIES_MAX
      case BitWidth.Of128 =>      FloatVector.SPECIES_128
      case BitWidth.Of256 =>      FloatVector.SPECIES_256
      case BitWidth.Of512 =>      FloatVector.SPECIES_512
      case BitWidth.Of64 =>       FloatVector.SPECIES_64
      case BitWidth.Preferred =>  FloatVector.SPECIES_PREFERRED
    }
  }
  
  given VectorSpeciesCreator[Double] with {
    override type SpeciesType = VectorSpecies[Boxed[Double]]

    override def create(bitWidth: BitWidth): SpeciesType = width match {
      case BitWidth.Maximum =>    DoubleVector.SPECIES_MAX
      case BitWidth.Of128 =>      DoubleVector.SPECIES_128
      case BitWidth.Of256 =>      DoubleVector.SPECIES_256
      case BitWidth.Of512 =>      DoubleVector.SPECIES_512
      case BitWidth.Of64 =>       DoubleVector.SPECIES_64
      case BitWidth.Preferred =>  DoubleVector.SPECIES_PREFERRED
    }
  }
}