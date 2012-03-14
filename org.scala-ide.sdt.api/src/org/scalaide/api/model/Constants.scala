package org.scalaide.api.model

trait Constants { this: Types =>

  abstract class AbsConstant {
    val value: Any
    def tpe: Type
    def isNaN: Boolean

    def booleanValue: Boolean
    def byteValue: Byte
    def shortValue: Short
    def charValue: Char
    def intValue: Int
    def longValue: Long
    def floatValue: Float
    def doubleValue: Double
    def stringValue: String
    def typeValue: Type
    def symbolValue: Symbol

    def convertTo(pt: Type): Constant
  }

  type Constant <: AbsConstant

  val Constant: ConstantExtractor

  abstract class ConstantExtractor {
    def apply(const: Any): Constant
    def unapply(arg: Constant): Option[Any]
  }
}
