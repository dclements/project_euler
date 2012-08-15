package dclements.pe;

import org.scalatest.FunSpec
import collection.immutable.TreeMap


class LargeIntSpec extends FunSpec {
  
  describe("A LargeInt") {
    it("should convert into a byteArray") {
      val i: LargeInt = LargeInt(Long.MaxValue) 
      
      expect(Array[Byte](127, -1, -1, -1, -1, -1, -1, -1).deep) {
        i.toByteArray.deep
      }
    }
    
    it("should convert from a large BigInt") {
      val i: LargeInt = BigInt("18446744073709551614")
      
      assert(i == LargeInt("18446744073709551614"))
    }
    
    it("should convert from a large negative BigInt") {
      val i: LargeInt = BigInt("-18446744073709551614")
      
      assert(i == LargeInt("-18446744073709551614"))
    }
    
    it("should not wrap during addition") {
      val i: LargeInt = LargeInt("9223372036854775807")+1
      
      assert(i > LargeInt(1))
    }
    
    it("should convert to a large BigInt") {
      val i: BigInt = LargeInt(2) * LargeInt(Long.MaxValue)
      
      assert(i == BigInt(2) * BigInt(Long.MaxValue))
    }
    
    it("should equal integers of the same size") {
      assert(5 == LargeInt(5))
    }
    
    it("should not equal integers of a different size") {
      assert(!(2 == LargeInt(5)))
    }
    
    it("should execute modular powers correctly") {
      val m = LargeInt("10000000000")
      
      expect(LargeInt("9700303872")) {
        LargeInt(2).modPow(7830457, m)
      }
    }
    
    it("should compare greater than") {
      expect(1) {
        LargeInt(5).compare(LargeInt(2))
      }
    }
    
    it("should compare less than") {
      expect(-1) {
        LargeInt(2).compare(LargeInt(5))
      }
    }
    
    it("should compare equals") {
      expect(0) {
        LargeInt(2).compare(LargeInt(2))
      }
    }
    
    it("should take a unary minus") {
      expect(LargeInt(-1)) {
        -LargeInt(1)
      }
    }
  }
  
  describe("A LargeIntOrdering") {
    it("should sort correctly") {
      val m = TreeMap[LargeInt, Int](LargeInt(3)->1, LargeInt(-3)->1, LargeInt(0)->1)
      
      expect(List[LargeInt](-3, 0, 3)) {
        m.keysIterator.toList
      }
    }
  }
}
