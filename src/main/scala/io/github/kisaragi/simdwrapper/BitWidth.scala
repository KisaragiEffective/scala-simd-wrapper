package io.github.kisaragi.simdwrapper

sealed trait BitWidth

object BitWidth {
  case object Of64 extends BitWidth with Sized[64]
  case object Of128 extends BitWidth with Sized[128]
  case object Of256 extends BitWidth with Sized[256]
  case object Of512 extends BitWidth with Sized[512]
  case object Maximum extends BitWidth
  case object Preferred extends BitWidth
}
