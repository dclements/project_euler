package dclements.pe;

import collection.immutable.ListMap
import annotation.tailrec

/**
 * Utilities for working with prime numbers. 
 */
object PrimeUtil {
  
  private val primeTests = 10
  
  lazy private val random = new scala.util.Random
  
  lazy private val primeSieve = PrimeSieve.primes()
  
  /**
   * Convenience method for primality testing. 
   * 
   * Note that jscience.LargeInteger.isProbablyPrime is not currently implemented, so
   * this code uses the BigInt implementation. 
   */
  def isPrime(n: BigInt): Boolean = n.isProbablePrime(primeTests)
  
  /**
   * Finds a nontrivial factor of a composite number.
   *
   * Implementation of the pollard rho algorithm:
   * http://en.wikipedia.org/wiki/Pollard's_rho_algorithm
   * 
   */
  def pollardRho(n: LargeInt): LargeInt = {

    require(n > 0)

    if (n == 1 || isPrime(n)) {
      n
    } else {
      val maxK = if (n > Int.MaxValue) Int.MaxValue else n.toInt
      val k = LargeInt((random.nextInt(maxK-3)+2))

      pollardRho(n, 2, 2, 1, (x: LargeInt) => x*x + k)
    }

  }
  
  @tailrec
  private def pollardRho(
      n: LargeInt,
      x: LargeInt,
      y: LargeInt,
      d: LargeInt,
      f: Function1[LargeInt, LargeInt]): LargeInt = {

    if (isPrime(n)) {
      n
    } else if (n % 2 == 0) {
      2
    } else if (d == 1) {
        val nx = f(x) % n
        val ny = f(f(y) % n) % n
        val nd = (nx - ny).gcd(n)

        val one = LargeInt(1)

        nd match {
          case `n` => pollardRho(n)
          case `one` => pollardRho(n, nx, ny, nd, f)
          case _ => nd
        }
    } else {
      d
    }

  }
  
  /**
   * Counts the number of times k divides into n.
   * 
   * @return (n = remainder->count = number of times it divided)
   */
  @tailrec
  private def countFactors(n: LargeInt, k: LargeInt, count: Int=0): Tuple2[LargeInt, Int] = {
    
    if (n % k == 0) {
      countFactors(n / k, k, count + 1)
    } else {
      (n, count)
    }
    
  }
  
  private def multiplyFactors(factors: Map[LargeInt, Int], n: Int): Map[LargeInt, Int] = {
    factors.map {case (k, v) => (k -> (v * n))}
  }

  private def addFactors(f1: Map[LargeInt, Int], f2: Map[LargeInt, Int]) = {
    f2 ++ f1.map {case (k: LargeInt, v: Int) => (k -> (v + f2.getOrElse(k, 0)))}
  }
  
  /**
   * Prime factor an integer n.
   *
   * @return a map of primes->exponents.
   */
  def primeFactor(
      n: LargeInt,
      retval: Map[LargeInt, Int]=Map[LargeInt, Int]()): Map[LargeInt, Int] = {
      
    val k = pollardRho(n)

    n match {
      case _ if n == 1 =>
        ListMap(retval.toList.sortBy(_._1):_*)
      case _ if isPrime(k) =>
        val factors = countFactors(n, k)
        
        primeFactor(factors._1, addFactors(retval,  Map(k->factors._2)))
      case _ =>
        val factors = countFactors(n, k)
        val primes = primeFactor(k)
        
        primeFactor(
          factors._1,
          addFactors(retval, multiplyFactors(primes, factors._2)))
    }
    
  }

  /**
  * Counts number of relatively prime integers that are less than n.
  * 
  * http://en.wikipedia.org/wiki/Euler's_totient_function
  */
  def eulerTotient(n: LargeInt): LargeInt = eulerTotient(primeFactor(n).toList, BigDecimal(n))
  
  @tailrec
  private def eulerTotient(f: List[Tuple2[LargeInt, Int]], retval: BigDecimal): LargeInt = {
  
    f match {
      case Nil => retval.toBigInt
      case ::(h, t) =>
        eulerTotient(
          t,
          retval * (1.0 - BigDecimal("1.0") / BigDecimal(h._1))
        )
    }
    
  }
  
  /**
   * Sum of the divisors of n raised to the power x.
   */
  def divisorFunction(n: LargeInt, x: Int): LargeInt =
    divisorFunction(primeFactor(n).toList, x)

  @tailrec
  private def divisorFunction(
      f: List[Tuple2[LargeInt, Int]],
      x: Int, r1: LargeInt=1,
      r2: LargeInt=1): LargeInt = {

    f match {
      case Nil => r1 / r2
      case ::(h, t) =>
        x match {
          case 0 => 
            divisorFunction(
              t,
              x,
              r1 * (h._2 + 1),
              r2)
          case _ =>
            divisorFunction(
              t,
              x,
              r1 * (h._1.pow((h._2 + 1) * x) - 1),
              r2 * (h._1.pow(x) - 1))
        }
    }
     
  }
  
  /**
   * Number of divisors of n.
   */
  def divisorCount(n: LargeInt): LargeInt = divisorFunction(n, 0)

  /**
   * Sum of divisors of n. 
   */
  def divisorSum(n: LargeInt): LargeInt = divisorFunction(n, 1)

  /**
   * Sum of the proper divisors of n.
   */
  def aliquotSum(n: LargeInt): LargeInt = divisorSum(n) - n
  
  /**
   * Converts a set of prime->exponent tuples into an integer.
   */
  @tailrec
  def reconstruct(n: List[Tuple2[LargeInt, Int]], r: LargeInt=LargeInt(1)): LargeInt = {
    
    n match {
      case Nil => r
      case ::(h, t) =>
        reconstruct(
          t,
          r * h._1.pow(h._2))
    }
    
  }

  /**
   * Find the nth prime. 
   */
  def nthPrime(n: Int): LargeInt = primeSieve take(n) last
  
  /**
   * Return all primes < n.
   */
  def primesUnder(n: LargeInt): List[LargeInt] =
    primeSieve takeWhile {(i: LargeInt) => i < n} toList
}


// http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
//http://voidmainargs.blogspot.com/2010/12/lazy-sieve-eratosthenes-in-scala-and.html

private object PrimeSieve {
  
  def primes() : Stream[LargeInt] =
    2 #:: sieve(3, collection.mutable.Map{ LargeInt(9) -> LargeInt(6) })
  
  private def sieve(
      p: LargeInt,
      pQ: collection.mutable.Map[LargeInt, LargeInt]): Stream[LargeInt] = 
    p #:: sieve(nextPrime(p + 2, pQ), pQ )
  
  private def nextComposite(
      x:LargeInt,
      step: LargeInt,
      pQ: collection.mutable.Map[LargeInt, LargeInt]): Unit = {
    
    pQ.get(x) match {
      case Some(_) => nextComposite(x + step, step, pQ)
      case None => pQ(x) = step
    } 
    
  }
  
  private def nextPrime(
      candidate: LargeInt,
      pQ: collection.mutable.Map[LargeInt, LargeInt]): LargeInt = {
  
    pQ.get(candidate) match {
      case Some(step) =>
        pQ -= candidate
        nextComposite(candidate + step, step, pQ) 
        nextPrime(candidate + 2, pQ)
      case None =>
        pQ(candidate * candidate) = candidate * 2
        candidate 
    }
    
  }
  
}
