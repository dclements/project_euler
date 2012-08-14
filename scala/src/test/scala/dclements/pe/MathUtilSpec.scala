package dclements.pe;

import org.scalatest.FunSpec

class MathUtilSpec extends FunSpec {
  describe("A Fibonacci Stream") {
    it("should have 55 as the 10th output") {
      expect((10->55)) {
        MathUtil.fibStream take 10 last
      }
    }
    
    it("should handle large numbers") {
      expect(4782) {
        (MathUtil.fibStream dropWhile {case (_, y) => y.digitLength < 1000} take 2 last)._1
      }
    }
  }
  
  describe("A Fibonacci function") {
    it("should have 55 as the 10th output") {
      expect(55) {
        MathUtil.fib(10)
      }
    }
    
    it("should handle large numbers") {
      expect(LargeInt("4346655768693745643568852767504062580256"
          + "466051737178040248172908953655541794905189040387984007925516929592259308032263"
          + "477520968962323987332247116164299644090653318793829896964992851600370447613779"
          + "5166849228875")) {
        MathUtil.fib(1000)
      }
    }
  }
  
  describe("A tail recursive Fibonacci function") {
    it("should have 55 as the 10th output") {
      expect(55) {
        MathUtil.fibt(10)
      }
    }
    
    it("should handle large numbers") {
      expect(LargeInt("4346655768693745643568852767504062580256"
          + "466051737178040248172908953655541794905189040387984007925516929592259308032263"
          + "477520968962323987332247116164299644090653318793829896964992851600370447613779"
          + "5166849228875")) {
        MathUtil.fibt(1000)
      }
    }
  }
  
  describe("A factorial function") {
    it("should handle large numbers") {
      expect(1404) {
        MathUtil.digitSum(MathUtil.factorial(200))
      }
    }
    
    it("should equal 720 at 6!") {
      expect(720) {
        MathUtil.factorial(6)
      }
    }
    
    it("should equal 1 at 0!") {
      expect(1) {
        MathUtil.factorial(0)
      }
    }
    
    it("should reject negative numbers") {
      intercept[IllegalArgumentException] {
        MathUtil.factorial(-1)
      }
    }
  }
  
  describe("A combinatorial function") {
    it("should manage probability for a deck of cards") {
      expect(2598960) {
        MathUtil.nCk(52, 5)
      }
    }
    
    it("should reject k > n") {
      intercept[IllegalArgumentException] {
        MathUtil.nCk(5, 25)
      }
    }
  }
  
  describe("A number reverser") {
    it("should be able to reverse 191") {
      expect(191) {
        MathUtil.reverse(191)
      }
    }
    
    it("should be able to reverse 196") {
      expect(691) {
        MathUtil.reverse(196)
      }
    }
    
    it("should be able to reverse 10000") {
      expect(1) {
        MathUtil.reverse(10000)
      }
    }
    
    it("should reject negative numbers") {
      intercept[IllegalArgumentException] {
        MathUtil.reverse(-10000)
      }
    }
  }
  
  describe("A palindrome function") {
    it("should work on single digit numbers") {
      expect(true) {
        MathUtil.isPalindrome(9)
      }
    }
    
    it("should work on multi-digit palindromic numbers") {
      expect(true) {
        MathUtil.isPalindrome(191)
      }
    }
    
    it("should work on multi-digit non-palindromic numbers") {
      expect(false) {
        MathUtil.isPalindrome(196)
      }
    }
    
    it("should factor in zeros") {
      expect(false) {
        MathUtil.isPalindrome(1000)
      }
    }
  }
}
