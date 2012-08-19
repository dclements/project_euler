package dclements.pe;

import annotation.tailrec

object MathUtil {

  private lazy val fibCache = collection.mutable.Map[LargeInt, LargeInt](
    (LargeInt(0)->LargeInt(0)),
    (LargeInt(1)->LargeInt(1)),
    (LargeInt(2)->LargeInt(1)),
    (LargeInt(3)->LargeInt(2)),
    (LargeInt(4)->LargeInt(3)),
    (LargeInt(5)->LargeInt(5)),
    (LargeInt(6)->LargeInt(8)),
    (LargeInt(7)->LargeInt(13)),
    (LargeInt(8)->LargeInt(21)),
    (LargeInt(9)->LargeInt(34)),
    (LargeInt(10)->LargeInt(55)),
    (LargeInt(11)->LargeInt(89)))
    
  private var factCache = collection.immutable.TreeMap[LargeInt, LargeInt](
    (LargeInt(0)->LargeInt(1)),
    (LargeInt(1)->LargeInt(1)),
    (LargeInt(2)->LargeInt(2)),
    (LargeInt(3)->LargeInt(6)),
    (LargeInt(4)->LargeInt(24)),
    (LargeInt(5)->LargeInt(120)))
  
  lazy val fibStream = FibStream.fibonacci()
  
  /**
   * n!
   * 
   * Uses memoization to minimize number of multiplications required. 
   */
  def factorial(n: LargeInt): LargeInt = {
  
    require(n >= 0)
    
    if (factCache.contains(n)) {
      return factCache(n)
    }
    
    val kval = factCache.maxBy {case (k, v) => k * (if (k < n) 1 else 0)}    
    val retval = kval._2 * partialFact(n, kval._1)
    
    factCache += (n->retval)
    retval
    
  }
  
  /**
   * Calculates n! / k!
   */
  def partialFact(n: LargeInt, k: LargeInt): LargeInt = {
  
    require(n >= k)
    (k+1 to n).view.par.foldLeft(LargeInt(1))(_ * _)
    
  }
  
  /**
   * Calculates (n k).
   */
  def nCk(n: LargeInt, k: LargeInt): LargeInt =
    partialFact(n, k) / factorial(n - k)
  
  /**
   * Convenience method for base**exponent % mod
   */
  def pow(base: LargeInt, exponent: LargeInt, mod: LargeInt): LargeInt =
    base.modPow(exponent, mod)
  
  /**
   *  Calculates the nth fibonacci number recursively, using memoization.
   */
  def fib(n: LargeInt): LargeInt = {
  
    if (fibCache.contains(n)) {
      return fibCache(n)
    } else if (fibCache.contains(n - 1)) {
      val retval = fibCache(n-1) + fib(n-2)
      
      fibCache += (n->retval)
      
      return retval
    } else if (fibCache.contains(n - 2)) {
      val retval = fib(n-1) + fibCache(n-2)
      
      fibCache += (n->retval)
      
      return retval
    }
    
    if (n > 100) {
      val retval = if (n % 2 == 0) {
        val k = n >> 1
        fib(k) * (2 * fib(k + 1) - fib(k))
      } else {
        val k = (n - 1) >> 1
        fib(k + 1) * fib(k + 1) + fib(k) * fib(k)
      }
      fibCache += (n->retval)
      retval
    } else {
      val retval = fib(n - 1) + fib(n - 2)
      fibCache += (n->retval)
      retval
    }
    
  }
  
  /**
   * A tail recursive fibonacci function.
   */
  @tailrec
  def fibt(
      n: LargeInt,
      pair1: Tuple2[LargeInt, LargeInt]=LargeInt(1)->LargeInt(1),
      pair2: Tuple2[LargeInt, LargeInt]=LargeInt(0)->LargeInt(1)): LargeInt = {
    
    if (n == 0) {
      pair2._1
    } else if (n % 2 == 0) {
      val squareFib = pair1._1 * pair1._1
      
      fibt(
        n >> 1,
        (2 * pair1._1 * pair1._2 - squareFib)->
          (squareFib + pair1._2 * pair1._2),
        pair2)
    } else {
      fibt(
        n-1,
        pair1,
        (pair1._1 * pair2._2 + pair2._1 * (pair1._2 - pair1._1))->
          (pair1._1 * pair2._1 + pair1._2 * pair2._2))
    }
    
  }
  
  /**
   * Reverses the digits in a number.
   */
  @tailrec
  def reverse(n: LargeInt, b: Int=10, r: LargeInt=0): LargeInt = {
  
    require(n >= 0)
    
    n match {
      case LargeInt.Zero => r
      case _ => reverse(n / b, b, b * r + n % b)
    }
    
  }
  
  /**
   * Detects whether a number is palindromic. 
   */
  def isPalindrome(n: LargeInt, b: Int=10): Boolean = n == reverse(n, b)
  
  /**
   * Sum of the digits of a number (base 10).
   */
  @tailrec
  def digitSum(n: LargeInt, retval: LargeInt=0): LargeInt = {
  
    n match {
      case LargeInt.Zero => retval
      case _ => digitSum(n / 10, retval + n % 10)
    }
    
  }

  /**
   * Determines if number + reverse(number) returns a palindrome within
   * k iterations.
   */
  @tailrec
  def lychrel(n: LargeInt, k: Int=50, count: Int=1): Boolean = {
  
    if (count > k) {
      return true
    }
    
    val v = n + reverse(n)
    if (isPalindrome(v)) {
      false
    } else {
      lychrel(v, k, count + 1)
    }

  }
  
  /**
   * Solves a continuous fraction. 
   */
  @tailrec
  def contFrac(
      an: List[Int],
      m1: Tuple2[LargeInt, LargeInt] = (LargeInt.One, LargeInt.Zero),
      m2: Tuple2[LargeInt, LargeInt] = (LargeInt.Zero, LargeInt.One)): Rational = {
      
    an match {
      case Nil =>
        Rational(m1._1, m1._2)
      case ::(h, t) =>
        contFrac(
          t,
          (h*m1._1 + m2._1, h*m1._2 + m2._2),
          m1)
    }
  }
  
  def isPermutation(a: LargeInt, b: LargeInt): Boolean =
    (a.toString sorted) equals (b.toString sorted)
   
}

private object FibStream {
  def fibonacci() : Stream[Tuple2[LargeInt, LargeInt]] = fibNext(1, 0, 1)
  
  private def fibNext(n1: LargeInt, n: LargeInt, ct: LargeInt): Stream[Tuple2[LargeInt, LargeInt]] = 
    (ct->n1) #:: fibNext(n1 + n, n1, ct + 1)
}
