package ga.kisaragi.simdwrapper

import jdk.incubator.vector._

import scala.reflect.ClassTag

object VectorOps {
  implicit final class VectorOp[E, V](self: V)(implicit vIsVector: V <:< Vector[E]) {
    def +(other: Vector[E]): Vector[E] = {
      self.add(other)
    }

    def -(other: Vector[E]): Vector[E] = {
      self.sub(other)
    }

    def *(other: Vector[E]): Vector[E] = {
      self.mul(other)
    }

    def /(other: Vector[E]): Vector[E] = {
      self.div(other)
    }

    def toArray(implicit ct: ClassTag[E]): Array[E] = {
      self.toArray.asInstanceOf[Array[E]]
    }

    def ==(other: Vector[E]): VectorMask[E] = {
      vIsVector(self).eq(other)
    }

    def <(other: Vector[E]): VectorMask[E] = {
      self.lt(other)
    }

    def apply(op: VectorOperators.Unary): Vector[E] = {
      self.lanewise(op)
    }

    def apply(op: VectorOperators.Test): VectorMask[E] = {
      self.test(op)
    }

    def unary_~(implicit l: V <:< IntVector): IntVector = {
      self.not()
    }

    // TODO: Non-functional way
    def apply[F](op: VectorOperators.Conversion[E, F], part: Int): Vector[F] = {
      self.convert(op, part)
    }

    def apply[F](op: VectorOperators.Conversion[E, F]): Vector[F] = {
      // NOTE: Magic constant "0" means that conversion is in-place and vector's size in bytes doesn't change.
      self.convert(op, 0)
    }
  }

  implicit final class IntVectorOp[V](self: V)(implicit vIsIntVec: V <:< IntVector) {
    def &[V2](other: V2)(implicit v2IsIntVec: V2 <:< IntVector): IntVector = {
      self.and(other)
    }

    def &(other: Int): IntVector = {
      self.and(other)
    }

    def |[V2](other: V2)(implicit v2IsIntVec: V2 <:< IntVector): IntVector = {
      self.or(other)
    }

    def |(other: Int): IntVector = {
      self.or(other)
    }

    def unary_~ : IntVector = {
      self.not()
    }

    def unary_- : IntVector = {
      self.neg()
    }
  }
  implicit final class ByteVectorOp[V](self: V)(implicit vIsByteVec: V <:< ByteVector) {
    // NOTE: Always fails.
    def viewAsFloatingLanes(implicit dummyImplicit: DummyImplicit): Nothing = self.viewAsFloatingLanes.asInstanceOf[Nothing]
  }

  implicit final class ShortVectorOp[V](self: V)(implicit vIsShortVec: V <:< ShortVector) {
    // NOTE: Always fails.
    def viewAsFloatingLanes(implicit dummyImplicit: DummyImplicit): Nothing = self.viewAsFloatingLanes.asInstanceOf[Nothing]
  }
}
