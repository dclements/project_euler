package dclements.pe;

import org.scalatest.FunSpec

class DiceSpec extends FunSpec {
  describe("A dice object") {
    it("should calculate variance") {
      expect(Rational(399, 1)) {
        Dice(12, 20).variance
      }
    }
    
    it("should calculate mean") {
      expect(Rational(126, 1)) {
        Dice(12, 20).mean
      }
    }
    
    it("should calculate sum of squares * p") {
      expect(Rational(21, 2)) {
        Dice(3, 6).mean
      }
    }
  } 
}
