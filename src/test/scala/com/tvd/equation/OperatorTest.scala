package com.tvd.equation

import com.tvd.equation.EquationConstant.BinaryOperator._
import com.tvd.equation.EquationConstant.UnaryOperator._
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OperatorTest extends AnyFlatSpec with Matchers {

  "None" should "not modify the argument" in {
    val tree = Leaf(None, -6)
    tree.value should be (-6)
  }

  "Implicit None" should "not modify the argument" in {
    val tree = Leaf(x=26)
    tree.value should be (26)
  }

  "7 negated" should "equal exact -7" in {
    val tree = Leaf(Negate, 7)
    tree.value should be (-7)
  }

  "0 negated" should "equal exact 0" in {
    val tree = Leaf(Negate, 0)
    tree.value should be (0)
  }

  "6 factorial" should "equal exact 720" in {
    val tree = Leaf(Factorial, 6)
    tree.value should be (720)
  }

  "0 factorial" should "equal exact 1" in {
    val tree = Leaf(Factorial, x=0)
    tree.value should be (1)
  }

  " minus 11 factorial" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Leaf(Factorial, -11))
    }
    exception.getMessage should equal ("Factorial of a negative number is not allowed !")
  }

  "5 square" should "equal exact 25" in {
    val tree = Leaf(Square, 5)
    tree.value should be (25)
  }

  "minus 4 square" should "equal exact 16" in {
    val tree = Leaf(Square, -4)
    tree.value should be (16)
  }

  "0 square" should "equal exact 0" in {
    val tree = Leaf(Square, 0)
    tree.value should be (0)
  }

  "81 square root " should "equal exact 9" in {
    val tree = Leaf(SquareRoot, 81)
    tree.value should be (9)
  }

  "-81 square root" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Leaf(SquareRoot, -81))
    }
    exception.getMessage should equal ("Square root of a negative number is not allowed !")
  }

  "79 square root" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Leaf(SquareRoot, 79))
    }
    exception.getMessage should equal ("Square root that doesn't return an integer is not allowed !")
  }

  "0 square root " should "equal exact 0" in {
    val tree = Leaf(SquareRoot, 0)
    tree.value should be (0)
  }

  "3 plus 8" should "equal exact 11" in {
    val tree = Branch(None, Addition, Leaf(x=3), Leaf(x=8))
    tree.value should be (11)
  }

  "negative 3 plus 8" should "equal exact 5" in {
    val tree = Branch(None, Addition, Leaf(Negate,x=3), Leaf(x=8))
    tree.value should be (5)
  }

  "negative 3 plus negative 8" should "equal exact minus 11" in {
    val tree = Branch(None, Addition, Leaf(Negate,x=3), Leaf(Negate,x=8))
    tree.value should be (-11)
  }

  "3 minus 1" should "equal exact 2" in {
    val tree = Branch(None, Subtraction, Leaf(x=3), Leaf(x=1))
    tree.value should be (2)
  }

  "3 minus minus 11" should "equal exact 14" in {
    val tree = Branch(None, Subtraction, Leaf(x=3), Leaf(Negate, x=11))
    tree.value should be (14)
  }

  "3 minus 11" should "equal minus 8" in {
    val tree = Branch(None, Subtraction, Leaf(x=3), Leaf(x=11))
    tree.value should be (-8)
  }

  "minus 3 minus minus 11" should "equal 8" in {
    val tree = Branch(None, Subtraction, Leaf(Negate, x=3), Leaf(Negate, x=11))
    tree.value should be (8)
  }

  "3 multiplied by 8" should "equal exact 24" in {
    val tree = Branch(None, Multiplication, Leaf(x=3), Leaf(x=8))
    tree.value should be (24)
  }

  "negative 3 multiplied by negative 8" should "equal exact 24" in {
    val tree = Branch(None, Multiplication, Leaf(Negate,x=3), Leaf(Negate,x=8))
    tree.value should be (24)
  }

  "negative 3 multiplied by 8" should "equal exact minus 24" in {
    val tree = Branch(None, Multiplication, Leaf(Negate,x=3), Leaf(x=8))
    tree.value should be (-24)
  }

  "0 multiplied by 8" should "equal exact 0" in {
    val tree = Branch(None, Multiplication, Leaf(x=0), Leaf(x=8))
    tree.value should be (0)
  }

  "negative 3 multiplied by 0" should "equal exact 0" in {
    val tree = Branch(None, Multiplication, Leaf(Negate,x=3), Leaf(x=0))
    tree.value should be (0)
  }

  "56 divided by 8" should "equal exact 7" in {
    val tree = Branch(None, Division, Leaf(x=56), Leaf(x=8))
    tree.value should be (7)
  }

  "56 divided by minus 8" should "equal exact minus 7" in {
    val tree = Branch(None, Division, Leaf(x=56), Leaf(Negate, x=8))
    tree.value should be (-7)
  }

  "minus 56 divided by 8" should "equal exact minus 7" in {
    val tree = Branch(None, Division, Leaf(Negate, x=56), Leaf(x=8))
    tree.value should be (-7)
  }

  "minus 56 divided by minus 8" should "equal exact 7" in {
    val tree = Branch(None, Division, Leaf(Negate, x=56), Leaf(Negate, x=8))
    tree.value should be (7)
  }

  "factorial of (minus 56 divided by minus 8)" should "equal exact 7" in {
    val tree = Branch(Factorial, Division, Leaf(Negate, x=56), Leaf(Negate, x=8))
    tree.value should be (5040)
  }

  "0 divided by minus 8" should "equal exact 0" in {
    val tree = Branch(None, Division, Leaf(x=0), Leaf(Negate, x=8))
    tree.value should be (0)
  }

  "56 divided by 11" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Division, Leaf(x=56), Leaf(x=11)))
    }
    exception.getMessage should equal ("Division that doesn't return an integer is not allowed !")
  }

  "56 divided by 0" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Division, Leaf(x=56), Leaf(x=0)))
    }
    exception.getMessage should equal ("Division by zero is not allowed !")
  }

  "3 to the power of 4" should "equal exact 81" in {
    val tree = Branch(None, Power, Leaf(x=3), Leaf(x=4))
    tree.value should be (81)
  }

  "9 to the power of minus 2" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Power, Leaf(x=9), Leaf(Negate, x=2)))
    }
    exception.getMessage should equal ("Division that doesn't return an integer is not allowed !")
  }

  "2 to the power of minus 3" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Power, Leaf(x=2), Leaf(Negate, x=3)))
    }
    exception.getMessage should equal ("Division that doesn't return an integer is not allowed !")
  }

  "The list of arguments" should "should contain 2 and 3" in {
      val args = Operator.arguments(Branch(None, Addition, Leaf(x=2), Leaf(Negate, x=3)))
    args should contain allOf (2, 3)
  }

  "The list of arguments" should "should contain 1, 2, 3 and 4" in {
    val args = Operator.arguments(Branch(None, Addition, Branch(None, Addition, Leaf(x=1), Leaf(Negate, x=2)), Branch(None, Addition, Leaf(x=3), Leaf(Negate, x=4))))
    args should contain allOf (1, 2, 3, 4)
  }

  "The list of operators" should "should contain +, - and /" in {
    val operators = Operator.operators(Branch(None, Addition, Branch(None, Subtraction, Leaf(x=1), Leaf(Negate, x=2)), Branch(None, Division, Leaf(x=3), Leaf(Negate, x=4))))
    operators should contain allOf (Addition, Subtraction, Division)
  }

  "minus 2 to the power of 3" should "equal exact minus 8" in {
    val tree = Branch(None, Power, Leaf(Negate, x=2), Leaf(x=3))
    tree.value should be (-8)
  }

  "minus 1 to the power of minus 3" should "equal exact minus 1" in {
    val tree = Branch(None, Power, Leaf(Negate, x=1), Leaf(Negate, x=3))
    tree.value should be (-1)
  }

  "0 to the power of minus 3" should "equal exact 0" in {
    val tree = Branch(None, Power, Leaf(x=0), Leaf(Negate, x=3))
    tree.value should be (0)
  }

  "0 to the power of 0" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Power, Leaf(x=0), Leaf(x=0)))
    }
    exception.getMessage should equal ("Zero to the power of zero is undefined since x^y as a function of 2 variables is not continuous at the origin !")
  }

  "14 to the power of 0" should "equal exact 1" in {
    val tree = Branch(None, Power, Leaf(x=14), Leaf(x=0))
    tree.value should be (1)
  }

  "minus 5 to the power of 0" should "equal exact 1" in {
    val tree = Branch(None, Power, Leaf(Negate,x=5), Leaf(x=0))
    tree.value should be (1)
  }

  "15 to the power of 8" should "exceed maximum value for Integers" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Power, Leaf(Negate,x=15), Leaf(x=8)))
    }
    exception.getMessage should equal ("Result exceeds maximum limit !")
  }

  "5 modulo 3" should "equal exact 2" in {
    val tree = Branch(None, Modulo, Leaf(x=5), Leaf(x=3))
    tree.value should be (2)
  }

  "Negate of (5 modulo 2)" should "equal exact minus 2" in {
    val tree = Branch(Negate, Modulo, Leaf(x=5), Leaf(x=3))
    tree.value should be (-2)
  }

  "5 modulo minus 3" should "equal exact 2" in {
    val tree = Branch(None, Modulo, Leaf(x=5), Leaf(Negate, x=3))
    tree.value should be (2)
  }

  "minus 5 modulo 3" should "equal exact minus 2" in {
    val tree = Branch(None, Modulo, Leaf(Negate, x=5), Leaf(x=3))
    tree.value should be (-2)
  }

  "minus 5 modulo minus 3" should "equal exact minus 2" in {
    val tree = Branch(None, Modulo, Leaf(Negate, x=5), Leaf(Negate, x=3))
    tree.value should be (-2)
  }

  "5 modulo 13" should "equal exact 5" in {
    val tree = Branch(None, Modulo, Leaf(None, x=5), Leaf(None, x=13))
    tree.value should be (5)
  }

  "0 modulo 0" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Modulo, Leaf(x=0), Leaf(x=0)))
    }
    exception.getMessage should equal ("Modulo of zero is undefined !")
  }

  "0 modulo 3" should "equal exact 0" in {
    val tree = Branch(None, Modulo, Leaf(x=0), Leaf(x=3))
    tree.value should be (0)
  }

  "5 modulo 0" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Operator.compute(Branch(None, Modulo, Leaf(x=5), Leaf(x=0)))
    }
    exception.getMessage should equal ("Modulo of zero is undefined !")
  }

  "Division that doesn't return an integer" should "be invalid" in {
    val tree = Branch(None, Division, Leaf(x=5), Leaf(x=3))
    Operator.computeSafe(tree) should be (0, false)
  }

  "Modulo by zero validation" should "be invalid" in {
    val tree = Branch(None, Modulo, Leaf(x=5), Leaf(x=0))
    Operator.computeSafe(tree) should be (0, false)
  }

  "Division by zero validation" should "be invalid" in {
    val tree = Branch(None, Division, Leaf(x=5), Leaf(x=0))
    Operator.computeSafe(tree) should be (0, false)
  }

  "Division that returns an integer validation" should "be valid" in {
    val tree = Branch(None, Division, Leaf(x=14), Leaf(x=7))
    Operator.computeSafe(tree) should be (2, true)
  }
}