package dclements.pe;

import collection.immutable.TreeMap
import org.scalatest.FunSpec

class RationalSpec extends FunSpec {
  
  describe("A Rational number") {
    
    it("should convert from a BigInt") {
      val i: Rational = BigInt(1)
      
      assert(i == 1)
    }
    
    it("should convert from a LargeInt") {
      val i: Rational = LargeInt(1)
      
      assert(i == 1)
    }
    
    it("should convert from a Long") {
      val i: Rational = 1
      
      assert(i == 1)
    }
    
    it("should equal integers of the same size") {
      assert(5 == Rational(5))
    }
    
    it("should not equal integers of a different size") {
      assert(!(2 == Rational(5)))
    }
    
    it("should compare greater than") {
      expect(1) {
        LargeInt(5).compare(LargeInt(2))
      }
    }
    
    it("should compare less than") {
      expect(-1) {
        Rational(2,3).compare(Rational(3,2))
      }
    }
    
    it("should compare equals") {
      expect(0) {
        Rational(2,3).compare(Rational(2,3))
      }
    }
    
    it("should take a unary minus") {
      expect(Rational(-1)) {
        -Rational(1)
      }
    }
  }
  
  describe("A LargeIntOrdering") {
    it("should sort correctly") {
      val m = TreeMap[Rational, Int](Rational(2, 3)->1, Rational(3,2)->1, Rational(0)->1)
      
      expect(List[Rational](Rational(0), Rational(2, 3), Rational(3, 2))) {
        m.keysIterator.toList
      }
    }
  }
}
