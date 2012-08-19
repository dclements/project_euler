package dclements.pe;

import collection.immutable.ListMap
import org.scalatest.FunSpec

class PrimeUtilSpec extends FunSpec {
  describe("A prime test") {
    it("should return true for the number 2") {
      expect(true) {
        PrimeUtil.isPrime(2)
      }
    }
    
    it("should return true for the negative number -5") {
      expect(true) {
        PrimeUtil.isPrime(-5)
      }
    }
    
    it("should return false for the number 9") {
      expect(false) {
        PrimeUtil.isPrime(9)
      }
    }
    
    it("should return false for the Carmichael number 340561") {
      expect(false) {
        PrimeUtil.isPrime(340561)
      }
    }
    
    it("should return true for a large Mersenne Prime") {
      expect(true) {
        PrimeUtil.isPrime(BigInt("618970019642690137449562111"))
      }
    }
  }
  
  describe("A pollard rho function") {
    it("should return a nontrivial factor for a power of 2") {
      val p = PrimeUtil.pollardRho(32)
      assert(p != 1)
      assert(32 % p == 0 )
    }
    
    it("should return a nontrivial factor for 340561") {
      val p = PrimeUtil.pollardRho(340561)
      assert(p != 1)
      assert(340561 % p == 0)
    }
    
    it("should throw an exception for negative values") {
      intercept[IllegalArgumentException] {
        PrimeUtil.pollardRho(-5)
      }
    }
    
    it("should throw an exception for 0") {
      intercept[IllegalArgumentException] {
        PrimeUtil.pollardRho(0)
      }
    }
    
    it("should return 1 for the argument 1") {
      assert(PrimeUtil.pollardRho(1) == 1)
    }
  }
  
  describe("A prime factoring function") {
    it("should factor a prime number into p->1") {
      expect(ListMap(5->1)) {
        PrimeUtil.primeFactor(5)
      }
    }
    
    it("should factor 340561") {
      expect(ListMap(13 -> 1, 17 -> 1, 23 -> 1, 67 -> 1)) {
        PrimeUtil.primeFactor(340561)
      }
    }
    
    it("should factor 57554809") {
      expect(ListMap(13 -> 3, 17 -> 1, 23 -> 1, 67 -> 1)) {
        PrimeUtil.primeFactor(57554809)
      }
    }
    
    it("should factor 359*2^32") {
      expect(ListMap(2 -> 32, 359 -> 1)) {
        PrimeUtil.primeFactor(LargeInt(2).pow(32) * 359)
      }
    }
    
    it("should return an empty map for 1") {
      expect(ListMap()) {
        PrimeUtil.primeFactor(1)
      }
    }
  }
  
  describe("Euler's totient function") {
    it("should work for 98") {
      expect(42) {
        PrimeUtil.eulerTotient(98)
      }
    }
    
    it("should work for a prime number") {
      expect(30) {
        PrimeUtil.eulerTotient(31)
      }
    }
  }
  
  describe("A divisor function") {
    it("should work for a prime number with a power of 0") {
      expect(2) {
        PrimeUtil.divisorFunction(7, 0)
      }
    }
  
    it("should work for a prime number with a power of 1") {
      expect(8) {
        PrimeUtil.divisorFunction(7, 1)
      }
    }
  
    it("should work for a prime number with a power of 2") {
      expect(50) {
        PrimeUtil.divisorFunction(7, 2)
      }
    }
    
    it("should work for a composite number with a power of 0") {
      expect(4) {
        PrimeUtil.divisorFunction(14, 0)
      }
    }
    
    it("should work for a composite number with a power of 1") {
      expect(24) {
        PrimeUtil.divisorFunction(14, 1)
      }
    }
    
    it("should work for a composite number with a power of 2") {
      expect(250) {
        PrimeUtil.divisorFunction(14, 2)
      }
    }
  }
  
  describe("A composite reconstruction function") {
    it("should reconstruct 57554809") {
      expect(57554809) {
        PrimeUtil.reconstruct(
          List(
            LargeInt(13) -> 3,
            LargeInt(17) -> 1,
            LargeInt(23) -> 1,
            LargeInt(67) -> 1))
      }
    }
    
    it("should reconstruct 340561") {
      expect(340561) {
        PrimeUtil.reconstruct(
          List(
            LargeInt(13) -> 1,
            LargeInt(17) -> 1,
            LargeInt(23) -> 1,
            LargeInt(67) -> 1))
      }
    }
  }
  
  describe("An nthPrime function") {
    it("should find that the fifth prime is 11") {
      expect(11) {
        PrimeUtil.nthPrime(5)
      }
    }
    
    it("should find that the 10001st prime is 104743") {
      expect(104743) {
        PrimeUtil.nthPrime(10001)
      }
    }
  }
  
  describe("A primesUnder function") {
    it("should be able to find the primes under 100") {
      expect(List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
          71, 73, 79, 83, 89, 97)) {
        PrimeUtil.primesUnder(100) 
      }
    }
    
    it("should not include n when n is prime") {
      expect(List(2, 3, 5)) {
        PrimeUtil.primesUnder(7)
      }
    }
  }
  
  describe("A nextPrime function") {
    it("should properly calculate the next prime from an odd number") {
      expect(7) {
        PrimeUtil.nextPrime(5)
      }
    }
    
    it("should properly calculate the next prime from an even number") {
      expect(11) {
        PrimeUtil.nextPrime(10)
      }
    }
    
    it("should skip early Carmichael numbers") {
      expect(340573) { // Skip 340561
        PrimeUtil.nextPrime(340559)
      }
    }
  }
}
