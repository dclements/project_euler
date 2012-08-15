package dclements.pe;

import org.jscience.mathematics.number.LargeInteger
import java.math.BigInteger
import scala.math._
import collection.immutable.NumericRange


object LargeInt {
  def apply(i: Short): LargeInt = new LargeInt(LargeInteger.valueOf(i.toLong))
  def apply(i: Int): LargeInt = new LargeInt(LargeInteger.valueOf(i.toLong))
  def apply(i: Long): LargeInt = new LargeInt(LargeInteger.valueOf(i))
  def apply(i: BigInteger): LargeInt = new LargeInt(LargeInteger.valueOf(i))
  def apply(i: String): LargeInt = new LargeInt(LargeInteger.valueOf(i))
  def apply(i: String, radix: Int): LargeInt = new LargeInt(LargeInteger.valueOf(i, radix))
  
  implicit def int2largeInt(i: Int): LargeInt = apply(i)
  implicit def long2largeInt(i: Long): LargeInt = apply(i)
  implicit def javaBigInteger2largeInt(i: BigInteger): LargeInt = apply(i)
  implicit def bigInt2largeInt(i: BigInt): LargeInt = apply(i.bigInteger)
  
  /**
   * Reverse conversion.
   */
  implicit def largeInt2bigInt(i: LargeInt): BigInt =
    if (i.isValidLong) BigInt(i.toLong) else BigInt(i.toByteArray)
  
  trait LargeIntOrdering extends Ordering[LargeInt] {
    def compare(x: LargeInt, y: LargeInt) = x.compare(y)
  }
  implicit object LargeInt extends LargeIntOrdering
  
  lazy val one: LargeInt = apply(1)
  lazy val zero: LargeInt = apply(0)
}

/**
 * Wrapper for org.jscience.mathematics.number.LargeInteger.
 */
class LargeInt(val largeInteger: LargeInteger) extends ScalaNumber with ScalaNumericConversions with Serializable {
  override def hashCode(): Int =
    if (isValidLong) unifiedPrimitiveHashcode else largeInteger.##
  
  protected def isWhole = true
  def underlying = largeInteger
    
  override def equals(that: Any): Boolean = that match {
    case that: LargeInt => this equals that
    case that: BigInt => this equals new LargeInt(LargeInteger.valueOf(that.bigInteger))
    case x => isValidLong && unifiedPrimitiveEquals(x)
  }
  
  override def isValidByte = this >= Byte.MinValue && this <= Byte.MaxValue
  override def isValidShort = this >= Short.MinValue && this <= Short.MaxValue
  override def isValidChar = this >= Char.MinValue && this <= Char.MaxValue
  override def isValidInt = this >= Int.MinValue && this <= Int.MaxValue
  def isValidLong = this >= (Long.MinValue + 1) && this <= (Long.MaxValue - 1)
  
  def equals(that: LargeInt): Boolean = compare(that) == 0
  def compare(that: LargeInt): Int = this.largeInteger.compareTo(that.largeInteger)
  
  def doubleValue = this.largeInteger.doubleValue
  def floatValue = this.largeInteger.floatValue
  def longValue = this.largeInteger.longValue
  def intValue = this.largeInteger.intValue
  override def byteValue = this.largeInteger.byteValue
  
  def <= (that: LargeInt): Boolean = compare(that) <= 0
  def >= (that: LargeInt): Boolean = compare(that) >= 0
  
  def < (that: LargeInt): Boolean = compare(that) < 0
  def > (that: LargeInt): Boolean = compare(that) > 0

  def + (that: LargeInt): LargeInt = new LargeInt(this.largeInteger.plus(that.largeInteger))
  def - (that: LargeInt): LargeInt = new LargeInt(this.largeInteger.minus(that.largeInteger))
  def unary_- : LargeInt = this.opposite()
  def * (that: LargeInt): LargeInt = new LargeInt(this.largeInteger.times(that.largeInteger))
  def / (that: LargeInt): LargeInt = new LargeInt(this.largeInteger.divide(that.largeInteger))
  def % (that: LargeInt): LargeInt = new LargeInt(this.largeInteger.mod(that.largeInteger))
  
  def << (n: Int): LargeInt = new LargeInt(this.largeInteger.shiftLeft(n))
  def >> (n: Int): LargeInt = new LargeInt(this.largeInteger.shiftRight(n))
  
  def abs: LargeInt = new LargeInt(this.largeInteger.abs())
  
  def bitLength: Int = this.largeInteger.bitLength()
  
  def digitLength: Int = this.largeInteger.digitLength()
  
  def gcd(that: LargeInt): LargeInt = new LargeInt(this.largeInteger.gcd(that.largeInteger))
  
  def lowestSetBit: Int = this.largeInteger.getLowestSetBit()
  
  def isEven: Boolean = this.largeInteger.isEven()
  
  def isLargerThan(i: LargeInt): Boolean = this.largeInteger.isLargerThan(i.largeInteger)
  
  def isNegative: Boolean = this.largeInteger.isNegative()
  
  def isOdd: Boolean = this.largeInteger.isOdd()
  
  def isPositive: Boolean = this.largeInteger.isPositive()
  
  def isPowerOfTwo: Boolean = this.largeInteger.isPowerOfTwo()
  
  def isProbablyPrime: Boolean = this.largeInteger.isProbablyPrime()
  
  def isZero: Boolean = this.largeInteger.isZero()
  
  def min(that: LargeInt): LargeInt = if (this < that) this else that
  def max(that: LargeInt): LargeInt = if (this < that) that else this
  
  def mod(that: LargeInt): LargeInt = new LargeInt(this.largeInteger.mod(that.largeInteger))
  
  def pow(exp: Int): LargeInt = new LargeInt(this.largeInteger.pow(exp))
  def modPow(exp: LargeInt, m: LargeInt): LargeInt =
    new LargeInt(this.largeInteger.modPow(exp.largeInteger, m.largeInteger))
  def modInverse(m: LargeInt): LargeInt = new LargeInt(this.largeInteger.modInverse(m.largeInteger))
  
  def opposite(): LargeInt = new LargeInt(this.largeInteger.opposite())
  
  def remainder(that: LargeInt): LargeInt = new LargeInt(this.largeInteger.remainder(that.largeInteger))
  
  def sqrt(): LargeInt = new LargeInt(this.largeInteger.sqrt())
  
  /**
   * Mimics the behavior of BigInt.toByteArray rather than the JScience version.
   */
  def toByteArray(): Array[Byte] = {
    
    val s = math.floor(this.bitLength / 8.0).toInt + 1
    val retval = new Array[Byte](s)
    
    this.largeInteger.toByteArray(retval, 0)
    
    retval
    
  }
  
  override def toString(): String = this.largeInteger.toString()
  def toString(radix: Int): String = this.largeInteger.toText(radix).toString()
}
