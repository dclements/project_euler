package dclements.pe;

import annotation.tailrec

/**
 * Represents a set of n dice with s sides. 
 */
object Dice {
  private val memo = collection.mutable.Map[Tuple2[Int, Int], Dice]()
  
  def apply(n: Int, s: Int): Dice = {
  
    if (memo.contains((n, s))) {
      memo((n, s))
    } else {
      val r = new Dice(n, s)
      
      memo += ((n, s)->r)
      
      r
    }
  
  }
}

class Dice(val n: LargeInt, val s: LargeInt) {
  private lazy val sn = s.pow(n.toInt)
  
  private val memo = collection.mutable.Map[LargeInt, Rational]()
  
  /**
   * Calculates P_{n,s}(k)
   */
  def apply(k: LargeInt): Rational = {
    
    if (memo.contains(k)) {
      memo(k)
    } else if (n == 1) {
      Rational(1, s)
    } else if (k > (s*n) || k < n) {
      Rational.Zero
    } else { 
    
      val pmax = math.floor((k.doubleValue - n.doubleValue)/s.doubleValue).longValue
      
      val nm = (0L to pmax).map((i)=>(
        (if (i % 2 == 0) LargeInt.One else -LargeInt.One) * MathUtil.nCk(n, i) *
        MathUtil.nCk(k - s*i - 1, n - 1))).foldLeft(LargeInt.Zero)(_ + _)
      val retval = Rational(nm, sn)
      
      memo += (k->retval)
      retval
    }
    
  }
  
  /**
   * Calculates E(X)
   */
  lazy val mean: Rational = {
    if (n == 1) {
      Rational(s + 1, 2) * n
    } else {
      Dice(1, s.toInt).mean * n
    }
  }
  
  /**
   * Calculates E(X^2)
   */
  lazy val sumOfSquares: Rational = {
    this.variance + this.mean * this.mean
  }
  
  /**
   * Calculates E(X^2) - E(X)^2
   */
  lazy val variance: Rational = {
    if (n == 1) {
      Rational(s * s - 1, 12)
    } else {
      Dice(1, s.toInt).variance * n
    }
  }
}

