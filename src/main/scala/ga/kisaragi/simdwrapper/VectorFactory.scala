package ga.kisaragi.simdwrapper
import jdk.incubator.vector._

object VectorFactory {
  // Fallback for types which are not Byte, Short, Int, Long, Float, nor Double.
  def apply[E](width: BitWidth, values: Array[E], offset: Int): Nothing =
    throw new UnsupportedOperationException("performed creation on unsupported type")
  def apply[E](width: BitWidth, values: Array[E], offset: Int, mask: VectorMask[E]): Nothing =
    throw new UnsupportedOperationException("performed creation on unsupported type")

  /*
  NOTE: reinterpretAsXxx is totally safe, is O(1), and is necessary because `species.fromArray`
  doesn't return unboxed version of Vector on signature.
  However, it `return`s `this` from unboxed versions.
  */
  /*
  NOTE: Below lines are generated following Ruby script:
  ```ruby
  types = %w(Byte Short Int Long Float Double)
  puts types.map {|t|
  <<EOS
    def apply(width: BitWidth, values: Array[#{t}], offset: Int): #{t}Vector = {
      val species = width match {
        case BitWidth.Maximum =>   #{t}Vector.SPECIES_MAX
        case BitWidth.Of128 =>     #{t}Vector.SPECIES_128
        case BitWidth.Of256 =>     #{t}Vector.SPECIES_256
        case BitWidth.Of512 =>     #{t}Vector.SPECIES_512
        case BitWidth.Of64 =>      #{t}Vector.SPECIES_64
        case BitWidth.Preferred => #{t}Vector.SPECIES_PREFERRED
      }
      species.fromArray(values, offset).reinterpretAs#{t}s()
    }
  EOS
  } * ?\n
  ```
  Please update this script if necessary.
  */
  def apply(width: BitWidth, values: Array[Byte], offset: Int): ByteVector = {
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

  def apply(width: BitWidth, values: Array[Short], offset: Int): ShortVector = {
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

  def apply(width: BitWidth, values: Array[Int], offset: Int): IntVector = {
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

  def apply(width: BitWidth, values: Array[Long], offset: Int): LongVector = {
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

  def apply(width: BitWidth, values: Array[Float], offset: Int): FloatVector = {
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

  def apply(width: BitWidth, values: Array[Double], offset: Int): DoubleVector = {
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
