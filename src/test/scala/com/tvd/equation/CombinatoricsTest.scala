package com.tvd.equation

import com.tvd.equation.EquationConstant.BinaryOperator._
import com.tvd.equation.EquationConstant.UnaryOperator._
import com.tvd.equation.EquationConstant._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class CombinatoricsTest extends AnyFlatSpec with Matchers {

  "Two operators (repetition allowed) and three arguments" should "generate the expected list of equations" in {
    val arguments = List(1, 2, 3)
    val operators = List(Addition, Subtraction)
    val generatedEquations = Combinatorics.equationPermutations(arguments, operators, distinct = false)
    val expectedEquations = List(
      List((None,None,None,1), (None,Addition,None,2), (None,Addition,None,3)),
      List((None,None,None,1), (None,Addition,None,2), (None,Subtraction,None,3)),
      List((None,None,None,1), (None,Subtraction,None,2), (None,Addition,None,3)),
      List((None,None,None,1), (None,Subtraction,None,2), (None,Subtraction,None,3)),
      List((None,None,None,1), (None,Addition,None,3), (None,Addition,None,2)),
      List((None,None,None,1), (None,Addition,None,3), (None,Subtraction,None,2)),
      List((None,None,None,1), (None,Subtraction,None,3), (None,Addition,None,2)),
      List((None,None,None,1), (None,Subtraction,None,3), (None,Subtraction,None,2)),
      List((None,None,None,2), (None,Addition,None,1), (None,Addition,None,3)),
      List((None,None,None,2), (None,Addition,None,1), (None,Subtraction,None,3)),
      List((None,None,None,2), (None,Subtraction,None,1), (None,Addition,None,3)),
      List((None,None,None,2), (None,Subtraction,None,1), (None,Subtraction,None,3)),
      List((None,None,None,2), (None,Addition,None,3), (None,Addition,None,1)),
      List((None,None,None,2), (None,Addition,None,3), (None,Subtraction,None,1)),
      List((None,None,None,2), (None,Subtraction,None,3), (None,Addition,None,1)),
      List((None,None,None,2), (None,Subtraction,None,3), (None,Subtraction,None,1)),
      List((None,None,None,3), (None,Addition,None,1), (None,Addition,None,2)),
      List((None,None,None,3), (None,Addition,None,1), (None,Subtraction,None,2)),
      List((None,None,None,3), (None,Subtraction,None,1), (None,Addition,None,2)),
      List((None,None,None,3), (None,Subtraction,None,1), (None,Subtraction,None,2)),
      List((None,None,None,3), (None,Addition,None,2), (None,Addition,None,1)),
      List((None,None,None,3), (None,Addition,None,2), (None,Subtraction,None,1)),
      List((None,None,None,3), (None,Subtraction,None,2), (None,Addition,None,1)),
      List((None,None,None,3), (None,Subtraction,None,2), (None,Subtraction,None,1)))

    generatedEquations should contain theSameElementsAs expectedEquations
  }

  "Three operators (repetition allowed) and four arguments" should "generate 648 equations" in {
    val arguments = List(7, 3, 2, 4)
    val operators = List(Addition, Subtraction, Multiplication)
    val generatedEquations = Combinatorics.equationPermutations(arguments, operators, distinct = false)
    generatedEquations.size should be (648)
  }

  "Six distinct operators and seven arguments" should "generate 86,400 equations" in {
    val arguments = List(1, 2, 3, 4, 5, 6)
    val operators = List(Addition, Subtraction, Multiplication,Division,Power)
    val generatedEquations = Combinatorics.equationPermutations(arguments, operators, distinct = true)
    generatedEquations.size should be (86400)
  }

  // operators permutations
  "Permutations of two repetitive operators" should "generate the expected list of equations" in {
    val basicEquation = List((None, None, None, 3), (None, None, None, 5), (None, None, None, 7))
    val operators = List( Addition, Subtraction)
    val generatedEquations = Combinatorics.operatorPermutations(operators, basicEquation, distinct = false)
    val expectedEquations = List(
      List((None,None,None,3), (None,Addition,None,5), (None,Addition,None,7)),
      List((None,None,None,3), (None,Addition,None,5), (None,Subtraction,None,7)),
      List((None,None,None,3), (None,Subtraction,None,5), (None,Addition,None,7)),
      List((None,None,None,3), (None,Subtraction,None,5), (None,Subtraction,None,7)))
    generatedEquations should contain theSameElementsAs expectedEquations
  }

  "Permutations of two non repetitive operators" should "generate the expected list of equations" in {
    val basicEquation = List((None, None, None, 3), (None, None, None, 5), (None, None, None, 7))
    val operators = List( Addition, Subtraction)
    val generatedEquations = Combinatorics.operatorPermutations(operators, basicEquation, distinct = true)
    val expectedEquations = List(
      List((None,None,None,3), (None,Addition,None,5), (None,Subtraction,None,7)),
      List((None,None,None,3), (None,Subtraction,None,5), (None,Addition,None,7)))
    generatedEquations should contain theSameElementsAs expectedEquations
  }

  "Permutations of one repetitive operator" should "generate 1 equations" in {
    val basicEquation = List((None, None, None, 1), (None, None, None, 2))
    val operators = List(Addition)
    val generatedEquations = Combinatorics.operatorPermutations(operators, basicEquation, distinct = false)
    generatedEquations.size should be (1)
  }

  "Permutations of two repetitive operators" should "generate  8 equations" in {
    val basicEquation = List((None, None, None, 1), (None, None, None, 2), (None, None, None, 3))
    val operators = List(Addition, Subtraction)
    val generatedEquations = Combinatorics.operatorPermutations(operators, basicEquation, distinct = false)
    generatedEquations.size should be (4)
  }

  "Permutations of six repetitive operators" should "generate 46656 equations" in {
    val basicEquation = List(
      (None, None, None, 1),
      (None, None, None, 3),
      (None, None, None, 4),
      (None, None, None, 5),
      (None, None, None, 6),
      (None, None, None, 7),
      (None, None, None, 8))
    val operators = List(Addition, Subtraction, Multiplication, Division, Power, Modulo)
    val generatedEquations = Combinatorics.operatorPermutations(operators, basicEquation, distinct = false)
    generatedEquations.size should be (46656)
  }

  "Permutations of six non repetitive operators" should "generate 720 equations" in {
    val basicEquation = List(
      (None, None, None, 1),
      (None, None, None, 3),
      (None, None, None, 4),
      (None, None, None, 5),
      (None, None, None, 6),
      (None, None, None, 7),
      (None, None, None, 8))
    val operators = List(Addition, Subtraction, Multiplication, Division, Power, Modulo)
    val generatedEquations = Combinatorics.operatorPermutations(operators, basicEquation, distinct = true)
    generatedEquations.size should be (720)
  }

  // arguments permutations
  "Permutations of two arguments" should "generate the expected list of equations" in {
    val arguments = List( 5, 7)
    val generatedEquations = Combinatorics.argumentPermutations(arguments)
    val expectedEquations = List(List((None, None, None, 5), (None, None, None, 7)), List((None, None, None, 7), (None, None, None, 5)))
    generatedEquations should contain theSameElementsAs expectedEquations
  }

  "Permutations of six arguments" should "generate 6! = 720 equations" in {
    val arguments = List( 1, 3, 5, 7, 9, 2)
    val generatedEquations = Combinatorics.argumentPermutations(arguments)
    generatedEquations.size should be (720)
  }

  // permutations
  "Permutations of two arguments" should "generate the expected list of results" in {
    val xs = List( 'a', 'b', 'c')
    val generatedPermutations = Combinatorics.permutations(xs)
    val expectedPermutations = List(List('a', 'b', 'c'), List('a', 'c', 'b'), List('b', 'a', 'c'), List('b', 'c', 'a'), List('c', 'a', 'b'), List('c', 'b', 'a'))
    generatedPermutations should contain theSameElementsAs expectedPermutations
  }

  "Permutations of four operators" should "generate 24 combinations" in {
    val xs = List(Addition, Subtraction, Multiplication, Division)
    val generatedPermutations = Combinatorics.permutations(xs)
    generatedPermutations.size should be (24)
  }

  // combinations
  "Combinations of two operators" should "generate the expected list of results" in {
    val xs = List( 'a', 'b')
    val generatedCombinations = Combinatorics.combinations(xs)
    val expectedCombinations = List(List('a', 'a'), List('a', 'b'), List('b', 'a'), List('b', 'b'))
    generatedCombinations should contain theSameElementsAs expectedCombinations
  }

  "Combinations of three operators" should "generate 27 combinations" in {
    val xs = List( 'a', 'b', 'c')
    val generatedCombinations = Combinatorics.combinations(xs)
    generatedCombinations.size should be (27)
  }

  "No operators provided" should "throw EquationException" in {
    val arguments = List(5, 7, 3, 2)
    val operators = List()
    val exception = intercept[EquationException] {
      Combinatorics.equationPermutations(arguments, operators, distinct = false)
    }
    exception.getMessage should equal ("At least one binary operator must be provided!")
  }

  "No arguments provided" should "throw EquationException" in {
    val arguments = List()
    val operators = List(Addition, Subtraction)
    val exception = intercept[EquationException] {
      Combinatorics.equationPermutations(arguments, operators, distinct = true)
    }
    exception.getMessage should equal ("At least two arguments for a binary operator must be provided!")
  }

  "Number of arguments not equal with (number of operators + 1)" should "throw EquationException" in {
    val arguments = List(5, 7, 3, 2)
    val operators = List(Addition, Subtraction, Multiplication, Division)
    val exception = intercept[EquationException] {
      Combinatorics.equationPermutations(arguments, operators, distinct = true)
    }
    exception.getMessage should equal ("The number of arguments must be equal with the (number of operators + 1)!")
  }

  "Undefined operator" should "throw EquationException" in {
    val arguments = List(5, 7, 9)
    val operators = List("UNDEFINED", Subtraction)
    val exception = intercept[EquationException] {
      Combinatorics.equationPermutations(arguments, operators, distinct = false)
    }
    exception.getMessage should equal ("Undefined operator. Allowed operators: [" + ListOfOperators.toString + "]")
  }

  "Arguments not in range" should "throw EquationException" in {
    val arguments = List(500000, 7, 9)
    val operators = List(Addition, Subtraction)
    val exception = intercept[EquationException] {
      Combinatorics.equationPermutations(arguments, operators, distinct = true)
    }
    exception.getMessage should equal ("Arguments must be in [-" + ArgumentMaximumValue + ", " + ArgumentMaximumValue + "] range!")
  }
}
