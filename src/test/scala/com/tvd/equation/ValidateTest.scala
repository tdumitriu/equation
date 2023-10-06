package com.tvd.equation

import com.tvd.equation.EquationConstant.BinaryOperator._
import com.tvd.equation.EquationConstant.ListOfOperators
import com.tvd.equation.EquationConstant.UnaryOperator._
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ValidateTest extends AnyFlatSpec with Matchers {

  "List of defined operators" should "equal the expected one" in {
    val expectedDefinedOperators = List(None, Negate, Square, SquareRoot, Factorial, Addition, Subtraction, Multiplication, Division, Power, Modulo)
    expectedDefinedOperators should contain theSameElementsAs ListOfOperators
  }

  "All arguments in range" should "return false" in {
    val arguments = List(5, -7, 3, 2)
    val notInRange = Validate.notInRange(arguments)
   notInRange should be (false)
  }

  "At least one argument out of range" should "return true" in {
    val arguments = List(5, 7, 3, 2000000)
    val notInRange = Validate.notInRange(arguments)
    notInRange should be (true)
  }

  "Many positive and negative arguments out of range" should "return true" in {
    val arguments = List(-5000, 7, 3, 2000000)
    val notInRange = Validate.notInRange(arguments)
    notInRange should be (true)
  }

  "At least one undefined operator" should "return false" in {
    val operators = List(None, Addition, "UNDEFINED")
    val undefinedOperator = Validate.definedOperator(operators)
    undefinedOperator should be (false)
  }

  "All defined operators" should "return true" in {
    val operators = List(None, Addition, Multiplication, Division, Power, Modulo)
    val undefinedOperator = Validate.definedOperator(operators)
    undefinedOperator should be (true)
  }
}
