package dclements.pe;

import org.jscience.mathematics.number.{Rational => JSRational, LargeInteger}
import scala.math.{ScalaNumber, ScalaNumericConversions}

import scala.language.implicitConversions

object Rational {
  def apply(num: LargeInt, den: LargeInt=LargeInt.One) =
    new Rational(JSRational.valueOf(num.largeInteger, den.largeInteger))
  
  implicit def int2rational(i: Int): Rational = apply(i)
  implicit def long2rational(i: Long): Rational = apply(i)
  implicit def largeInt2rational(i: LargeInt): Rational =
    apply(i)
  implicit def bigInt2rational(i: BigInt): Rational =
    apply(i)
  
  trait RationalOrdering extends Ordering[Rational] {
    def compare(x: Rational, y: Rational) = x.compare(y)
  }
  implicit object Rational extends RationalOrdering
  
  lazy val MinusOne = apply(-1)
  lazy val One = apply(1)
  lazy val OneFifth = apply(1, 5)
  lazy val OneHalf = apply(1, 2)
  lazy val OneQuarter = apply(1, 4)
  lazy val OneThird = apply(1, 3)
  lazy val Zero = apply(0)
  lazy val TwoThirds = apply(2, 3)
}

class Rational(val rational: JSRational) extends ScalaNumber with ScalaNumericConversions with Serializable {
  override def hashCode(): Int = rational.##
  
  override def isWhole = divisor == 1
  override def underlying = rational
    
  override def equals(that: Any): Boolean = that match {
    case that: Rational => this equals that
    case that: LargeInt => this equals Rational(that, LargeInt.One)
    case x => isValidLong && unifiedPrimitiveEquals(x)
  }
  
  override def isValidByte = isWhole && this >= Byte.MinValue && this <= Byte.MaxValue
  override def isValidShort = isWhole && this >= Short.MinValue && this <= Short.MaxValue
  override def isValidChar = isWhole && this >= Char.MinValue && this <= Char.MaxValue
  override def isValidInt = isWhole && this >= Int.MinValue && this <= Int.MaxValue
  def isValidLong = isWhole && this >= (Long.MinValue + 1) && this <= (Long.MaxValue - 1)
  
  def dividend: LargeInt = new LargeInt(rational.getDividend())
  def divisor: LargeInt = new LargeInt(rational.getDivisor())
  
  def equals(that: Rational): Boolean = compare(that) == 0
  def compare(that: Rational): Int = this.rational.compareTo(that.rational)
  
  def doubleValue = this.rational.doubleValue
  def floatValue = this.rational.floatValue
  def longValue = this.rational.longValue
  def intValue = this.rational.intValue
  override def byteValue = this.rational.byteValue
  
  def <= (that: Rational): Boolean = compare(that) <= 0
  def >= (that: Rational): Boolean = compare(that) >= 0
  
  def < (that: Rational): Boolean = compare(that) < 0
  def > (that: Rational): Boolean = compare(that) > 0
  
  def + (that: Rational): Rational = new Rational(this.rational.plus(that.rational))
  def - (that: Rational): Rational = new Rational(this.rational.minus(that.rational))
  def unary_- : Rational = this.opposite()
  def * (that: Rational): Rational = new Rational(this.rational.times(that.rational))
  def / (that: Rational): Rational = new Rational(this.rational.divide(that.rational))
  
  def abs: Rational = new Rational(this.rational.abs())
  
  def isLargerThan(i: Rational): Boolean = this.rational.isLargerThan(i.rational)
  
  def isNegative: Boolean = this.rational.isNegative()
  
  def isPositive: Boolean = this.rational.isPositive()
  
  def isZero: Boolean = this.rational.isZero()
  
  def min(that: Rational): Rational = if (this < that) this else that
  def max(that: Rational): Rational = if (this < that) that else this
  
  def pow(exp: Int): Rational = new Rational(this.rational.pow(exp))
  
  def opposite(): Rational = new Rational(this.rational.opposite())
  
  override def toString(): String = this.rational.toString()
}
