package com.tvd.equation

import com.tvd.equation.Catalan._
import com.tvd.equation.EquationConstant.BinaryOperator._
import com.tvd.equation.EquationConstant.OperatorGroup._
import com.tvd.equation.EquationConstant.UnaryOperator._
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ServiceTest extends AnyFlatSpec with Matchers {

  "The equation for tree1" should "be as expected" in {
    val tree1 = Branch(None, Multiplication, Branch(None, Addition, Leaf(None, 3), Leaf(None, 5)), Branch(None, Subtraction, Leaf(None, 1), Leaf(None, 4)))
    val equation = tree1.text + " = " + Operator.compute(tree1)
    equation should be ("((3+5)*(1-4)) = -24")
  }
  "The equation for tree2" should "be as expected" in {
    val tree2 = Branch(None, Multiplication, Leaf(None, 5), Leaf(None, 2))
    val equation = tree2.text + " = " + Operator.compute(tree2)
    equation should be ("(5*2) = 10")
  }
  "The equation for tree3" should "be as expected" in {
    val tree3 = Leaf(Factorial, 5)
    val equation = tree3.text + " = " + Operator.compute(tree3)
    equation should be ("!5 = 120")
  }
  "The equation for tree4" should "be as expected" in {
    val tree4 = Branch(None, Addition, Branch(None, Multiplication,Branch(None, Subtraction, Leaf(Square, 3), Leaf(None, 7)),Branch(None, Subtraction,Branch(None, Power, Leaf(None, 2), Leaf(None, 3)),Leaf(Negate, 7))),Branch(None, Subtraction, Branch(None, Division, Leaf(None, 12), Leaf(None, 3)),Leaf(SquareRoot, 25)))
    val equation = tree4.text + " = " + Operator.compute(tree4)
    equation should be ("(((3²-7)*((2^3)-(-7)))+((12/3)-√25)) = 29")
  }

  "The equation for tree5" should "be as expected" in {
    val equation = Leaf(Square, 5).text
    equation should be ("5²")
  }

  "The number of generated trees with 10 nodes" should "equal exact 16,796" in {
    val allTrees = Catalan.generateAllTrees(10)
    allTrees.size should be (16796)
  }

  "The generated trees with three nodes with no input" should "be the expected ones" in {
    val generatedTrees = Catalan.generateAllTrees(3)
    val expectedTrees = List(
      Branch("","",Leaf("",0),Branch("","",Leaf("",0),Branch("","",Leaf("",0),Leaf("",0)))),
      Branch("","",Leaf("",0),Branch("","",Branch("","",Leaf("",0),Leaf("",0)),Leaf("",0))),
      Branch("","",Branch("","",Leaf("",0),Leaf("",0)),Branch("","",Leaf("",0),Leaf("",0))),
      Branch("","",Branch("","",Leaf("",0),Branch("","",Leaf("",0),Leaf("",0))),Leaf("",0)),
      Branch("","",Branch("","",Branch("","",Leaf("",0),Leaf("",0)),Leaf("",0)),Leaf("",0)))
    generatedTrees should contain theSameElementsAs expectedTrees
  }

  "The generated and populated trees with three nodes and with input" should "be the expected ones" in {
    val input = List((None,None,None,1),(None,Addition,None,2),(None,Subtraction,None,3),(None,Multiplication,None,4))
    val generatedTrees = Catalan.generateAllTrees(input)
    val expectedTrees = List(
      Branch("","+",Leaf("",1),Branch("","-",Leaf("",2),Branch("","*",Leaf("",3),Leaf("",4)))),
      Branch("","+",Leaf("",1),Branch("","-",Branch("","*",Leaf("",2),Leaf("",3)),Leaf("",4))),
      Branch("","+",Branch("","-",Leaf("",1),Leaf("",2)),Branch("","*",Leaf("",3),Leaf("",4))),
      Branch("","+",Branch("","-",Leaf("",1),Branch("","*",Leaf("",2),Leaf("",3))),Leaf("",4)),
      Branch("","+",Branch("","-",Branch("","*",Leaf("",1),Leaf("",2)),Leaf("",3)),Leaf("",4)))
    generatedTrees should contain theSameElementsAs expectedTrees
  }

  "The catalan number for 16 nodes" should "equal exact 35,357,670" in {
    val catalanNumber = Catalan.catalanNumber(16)
    catalanNumber should be (35357670)
  }

  "The average of 1, 6, 8" should "be 5" in {
    val summary = Map(1 -> List(1,2,3), 6 -> List(1,2,3), 8 -> List(1,2,3))
    val average = Service.getSummaryAverage(summary)
    val expectedAverage = 5
    average should be (expectedAverage)
  }

  "The median of 1, 6, 8, 17, 99" should "be 8" in {
    val summary = Map(1 -> List(1,2,3), 6 -> List(1,2,3), 8 -> List(1,2,3), 17 -> List(1,2,3), 99 -> List(1,2,3))
    Service.getSummaryMedian(summary) should be (8)
  }

  "The maximum of 1, 6, 8, 17, 99" should "be 99" in {
    val summary = Map(1 -> List(1,2,3), 6 -> List(1,2,3), 8 -> List(1,2,3), 17 -> List(1,2,3), 99 -> List(1,2,3))
    Service.getSummaryMaximum(summary) should be (99)
  }

  "The minimum of 1, 6, 8, 17, 99" should "be 1" in {
    val summary = Map(1 -> List(1,2,3), 6 -> List(1,2,3), 8 -> List(1,2,3), 17 -> List(1,2,3), 99 -> List(1,2,3))
    Service.getSummaryMinimum(summary) should be (1)
  }

  "The distribution of 1, 6, 8, 17, 99" should "be 3" in {
    val summary = Map(1 -> List(1,2), 6 -> List(1,2,3,4,5), 8 -> List(1,2,3), 17 -> List(1), 99 -> List(1,2,3,5,6))
    Service.getSummaryDistribution(summary).asInstanceOf[Double] should be (3.2 +- 0.01)
  }

  "The statistics for mixed values with positive closer to zero" should "be the expected ones" in {
    val element: (Int, List[Int]) = (7, List(-11, -4, 3, 7, 15, 26))
    val statistics = Service.getSummaryElementStatistics(element)
    statistics._1 should be (7)
    statistics._2 should be (6)
    statistics._3 should be (-11)
    statistics._4 should be (26)
    statistics._5 should be (6.0 +- 0.01)
    statistics._6 should be (7)
    statistics._7 should be (3)
  }

  "The statistics for mixed values with negative closer to zero" should "be the expected ones" in {
    val element: (Int, List[Int]) = (7, List(-11, -1, 3, 7, 15, 26))
    val statistics = Service.getSummaryElementStatistics(element)
    statistics._1 should be (7)
    statistics._2 should be (6)
    statistics._3 should be (-11)
    statistics._4 should be (26)
    statistics._5 should be (6.5 +- 0.01)
    statistics._6 should be (7)
    statistics._7 should be (3)
  }

  "The statistics for only positive values" should "be the expected ones" in {
    val element: (Int, List[Int]) = (29, List(3, 9, 17, 25, 33))
    val statistics = Service.getSummaryElementStatistics(element)
    statistics._1 should be (29)
    statistics._2 should be (5)
    statistics._3 should be (3)
    statistics._4 should be (33)
    statistics._5 should be (17.4 +- 0.01)
    statistics._6 should be (17)
    statistics._7 should be (3)
  }

  "The statistics for only negative values" should "be the expected ones" in {
    val element: (Int, List[Int]) = (33, List(-11, -9, -7, -5, -3, -2))
    val statistics = Service.getSummaryElementStatistics(element)
    statistics._1 should be (33)
    statistics._2 should be (6)
    statistics._3 should be (-11)
    statistics._4 should be (-2)
    statistics._5 should be (-6.16 +- 0.01)
    statistics._6 should be (-5)
    statistics._7 should be (-2)
  }

  "All randomly generated list of arguments" should "have distinct elements" in {
    var sameSize = true
    for(i <- 1 to 100) {
      if(Service.generateRandomArguments(5, 8).size != 5) sameSize = false
    }
    sameSize should be (true)
  }

  "Generatation of 'Any number of' operators from pool #0" should "return Addition and Subtraction" in {
    val operators = Service.generateRandomOperators(0, 13)
    operators should be (List(Addition, Subtraction))
  }

  "Generation of 2 operators from pool #1" should "return 2 operators from pool" in {
    val operators = Service.generateRandomOperators(1, 2)
    operators should contain atLeastOneOf (Addition, Subtraction, Multiplication)
  }

  "Generation of 3 operators from pool #1" should "return 3 operators from pool" in {
    val operators = Service.generateRandomOperators(1, 3)
    operators should be (List(Addition, Subtraction, Multiplication))
  }

  "Generation of random 2 operators from pool #3" should "return 2 operators from pool #3" in {
    val operators = Service.generateRandomOperators(og3, 2)
    operators should contain atLeastOneOf (Addition, Subtraction, Division)
  }

  "Generation of random 2 operators from pool #1" should "return 2 operators from pool #1" in {
    val operators = Service.generateRandomOperators(og1, 2)
    operators should contain allOf (Addition, Subtraction)
  }

  "Generation of random 4 operators from pool #3" should "return all 3 operators from pool #3" in {
    val operators = Service.generateRandomOperators(og3, 5)
    operators should contain allOf (Addition, Subtraction, Division)
  }


  "Generation of random 3 operators from pool #4" should "return 3 operators from pool #4" in {
    val operators = Service.generateRandomOperators(og4, 3)
    operators should contain atLeastOneOf (Addition, Subtraction, Multiplication, Division)
  }

  "Generation of random 4 operators from pool #6" should "return 4 operators from pool #6" in {
    val operators = Service.generateRandomOperators(og6, 4)
    operators should contain atLeastOneOf (Addition, Subtraction, Multiplication, Division, Power, Modulo)
  }

  "Combinations of 5 taken as 3" should "return 10" in {
    Service.combinationOf(5, 3) should be (10)
  }

  "Combinations of 3 taken as 2" should "return 3" in {
    Service.combinationOf(3, 2) should be (3)
  }

  "Combinations of 3 taken as 3" should "return 1" in {
    Service.combinationOf(3, 3) should be (1)
  }

  "Combinations of 3 taken as 0" should "return 1" in {
    Service.combinationOf(3, 0) should be (1)
  }

  "Combinations of 6 taken as 3" should "return 20" in {
    Service.combinationOf(6, 3) should be (20)
  }

  "Combinations of 2 taken as 3" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Service.combinationOf(2, 3) should be (1)
    }
    exception.getMessage should equal ("Size of combination pool should be greater than the group size of combinations!")
  }

  "Combinations of 2 taken as -1" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Service.combinationOf(2, -1) should be (1)
    }
    exception.getMessage should equal ("The group size of combinations should be positive!")
  }

  "Combinations of -2 taken as -1" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Service.combinationOf(-2, -1) should be (1)
    }
    exception.getMessage should equal ("Size of combination pool should be greater than 0!")
  }

  "Combinations of -2 taken as 1" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Service.combinationOf(-2, 1) should be (1)
    }
    exception.getMessage should equal ("Size of combination pool should be greater than 0!")
  }

  "A random position in a list of 3 elements" should "be a value from list of (0, 1, 2)" in {
    Some(Service.getRandomPosition(3)) should contain oneOf (0, 1, 2)
  }

  "A random position in a list of 0 elements" should "be 0" in {
    Service.getRandomPosition(0) should be (0)
  }

  "A random position in a list of negative size elements" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Service.getRandomPosition(-3) should be (1)
    }
    exception.getMessage should equal ("Range of random value should be positive!")
  }

  "Factorial of 5" should "be 120" in {
    factorial(5) should be (120)
  }

  "Factorial of 0" should "be 1" in {
    factorial(0) should be (1)
  }

  "Factorial of -3" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      factorial(-3) should be (1)
    }
    exception.getMessage should equal ("Factorial of a negative number is undefined!")
  }

  "Factorial of 13" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      factorial(33) should be (120)
    }
    exception.getMessage should equal ("This result is irrelevantly big for the current application!")
  }

  "Equation with injected delimiters" should "be ((7+(2/(5-3)))|*|4)" in {
    val equationWithDelimiters = Validate.insertDelimiters("((7+(2/(5-3)))*4)".toList)
    val expectedChars = "(7+(2/(5-3)))|*|4".toList
    equationWithDelimiters should contain theSameElementsAs expectedChars
  }

  "Equation with injected delimiters" should "be (9|-|((5/(4*1))+3))" in {
    val equationWithDelimiters = Validate.insertDelimiters("(9-((5/(4*1))+3))".toList)
    val expectedChars = "9|-|((5/(4*1))+3)".toList
    equationWithDelimiters should contain theSameElementsAs expectedChars
  }

  "Equation with injected delimiters" should "be (9|-|7)" in {
    val equationWithDelimiters = Validate.insertDelimiters("(9-7)".toList)
    val expectedChars = "9|-|7".toList
    equationWithDelimiters should contain theSameElementsAs expectedChars
  }

  "Equation with injected delimiters" should "be (7+(2/(5-3)))|-|((5/(4*1))+3)" in {
    val equationWithDelimiters = Validate.insertDelimiters("((7+(2/(5-3)))-((5/(4*1))+3))".toList)
    val expectedChars = "(7+(2/(5-3)))|-|((5/(4*1))+3)".toList
    equationWithDelimiters should contain theSameElementsAs expectedChars
  }

  "Equation with injected delimiters" should "be (17+(28/(5-13)))|-|((5/(124*1))+83)" in {
    val equationWithDelimiters = Validate.insertDelimiters("((17+(28/(5-13)))-((5/(124*1))+83))".toList)
    val expectedChars = "(17+(28/(5-13)))|-|((5/(124*1))+83)".toList
    equationWithDelimiters should contain theSameElementsAs expectedChars
  }

  "Generated tree from (9-3)" should "be the same one" in {
    val generatedTree: Tree = Service.generateTree("(9-3)".toList)
    generatedTree.text should be ("(9-3)")
  }

  "Generated tree from (9-(-3))" should "be (9-(0-3))" in {
    val generatedTree: Tree = Service.generateTree("(9-(0-3))".toList)
    generatedTree.text should be ("(9-(0-3))")
  }

  "Generated tree from (2^3))" should "be (2^3)" in {
    val generatedTree: Tree = Service.generateTree("(2^3)".toList)
    generatedTree.text should be ("(2^3)")
  }

//  "Generated tree from (2+!3))" should "be (2+!3)" in {
//    val generatedTree: Tree = Service.generateTree("(2+!3)".toList)
//    generatedTree.text should be ("(2+!3)")
//  }

  "Generated tree from (9-((5/(4*1))+3))" should "be the same one" in {
    val generatedTree: Tree = Service.generateTree("(9-((5/(4*1))+3))".toList)
    generatedTree.text should be ("(9-((5/(4*1))+3))")
  }

  "Generated tree from (((5/(4*1))+3)-7)" should "be the same one" in {
    val generatedTree: Tree = Service.generateTree("(((5/(4*1))+3)-7)".toList)
    generatedTree.text should be ("(((5/(4*1))+3)-7)")
  }

  "Generated tree from (((7+(24/(5-3)))*4)-((5/(4*1))+3))" should "be the same one" in {
    val generatedTree: Tree = Service.generateTree("(((7+(24/(5-3)))*4)-((5/(4*1))+3))".toList)
    generatedTree.text should be ("(((7+(24/(5-3)))*4)-((5/(4*1))+3))")
  }

  "Parenthesis validation for (((7+(24/(5-3)))*4)-((5/(4*1))+3))" should "be true" in {
    val valid = Validate.validateParenthesis("(((7+(24/(5-3)))*4)-((5/(4*1))+3))".toList)
    valid should be (true)
  }

  "Parenthesis validation for (((7+(24/(5-3)))*4)-((5/(4*1) ->!)!<- )+3))" should "be false" in {
    val valid = Validate.validateParenthesis("(((7+(24/(5-3)))*4)-((5/(4*1)))+3))".toList)
    valid should be (false)
  }

  "Parenthesis validation for )((7+(24/(5-3)))*4)-((5/(4*1))+3))(" should "be false" in {
    val valid = Validate.validateParenthesis(")(((7+(24/(5-3)))*4)-((5/(4*1))+3))(".toList)
    valid should be (false)
  }

  "Validation of input equation ((5/(4*1))+3)" should "pass" in {
    val equation = Validate.validateEquation("  ((5/(4 *1)  )+ 3)")
    equation should have size 13
  }

  "Validation of input equation ((5/(a*1))+3)" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Validate.validateEquation("((5/(a*1))+3)")
    }
    exception.getMessage should equal ("Invalid character [a] found in equation!")
  }

  "Validation of input equation ((5/(4*1)))+3)" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Validate.validateEquation("((5/(4*1)))+3)")
    }
    exception.getMessage should equal ("Parenthesises are not balanced!")
  }

  "Validation of input equation ((5/(4?1)))+3)" should "throw EquationException" in {
    val exception = intercept[EquationException] {
      Validate.validateEquation("((5/(4?1))+3)")
    }
    exception.getMessage should equal ("Invalid character [?] found in equation!")
  }

  "List of two closest to zero positive integers" should "return 11 and 22" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -12, 11, 15, 19),
      4 -> List(22, 23, 27),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 2, onlyPositives = true) should
      contain theSameElementsAs List((2, 11), (4, 22))
  }

  "List of two closest to zero integers" should "return 11 and 22" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -12, 11, 15, 19),
      4 -> List(22, 23, 27),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 2, onlyPositives = false) should
      contain theSameElementsAs List((2, 11), (4, 22))
  }

  "List of two closest to zero positive integers when there are no positive integers" should "return -12 and 22" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -12),
      4 -> List(22, 23, 27),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 2, onlyPositives = true) should
      contain theSameElementsAs List((2, -12), (4, 22))
  }

  "List of two closest to zero integers when there are no positive integers" should "return -2 and 22" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -2),
      4 -> List(22, 23, 27),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 2, onlyPositives = false) should
      contain theSameElementsAs List((2, -2), (4, 22))
  }

  "List of two closest to zero integers" should "return -2 and 22" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -2, 11, 15, 19),
      4 -> List(22, 23, 27),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 2, onlyPositives = false) should
      contain theSameElementsAs List((2, -2), (4, 22))
  }

  "List of four closest to zero integers" should "return -2, 22, and -31" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -2, 11, 15, 19),
      4 -> List(22, 23, 27),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 4, onlyPositives = false) should
      contain theSameElementsAs List((2, -2), (4, 22), (7, -31))
  }

  "List of three closest to zero positive integers" should "return -2, -22, and 35" in {
    val map: Map[Int, List[Int]] = Map(
      2 -> List(-17, -13, -2),
      4 -> List(-22),
      7 -> List(-31, 35, 39))
    Service.getEquationWinners(map, 4, onlyPositives = true) should
      contain theSameElementsAs List((2, -2), (4, -22), (7, 35))
  }

  "List of two closest to zero integers when no summary exists" should "return empty list" in {
    val map: Map[Int, List[Int]] = Map()
    Service.getEquationWinners(map, 2, onlyPositives = false) should be ('empty)
  }

  "The number of commutative operators for a string equation" should "be 2" in {
    val equation = "((5/(4*1)))+3)"
    Service.getNumberOfCommutativeOperators(equation) should be (2)
  }

  "The number of commutative operators for a string equation" should "be 0" in {
    val equation = "((5/(4-1)))^3)"
    Service.getNumberOfCommutativeOperators(equation) should be (0)
  }

  "The number of commutative operators for a Tree equation" should "be 2" in {
    val tree = Branch(None, Addition, Branch(None, Multiplication,Branch(None, Subtraction, Leaf(Square, 3), Leaf(None, 7)),Branch(None, Subtraction,Branch(None, Power, Leaf(None, 2), Leaf(None, 3)),Leaf(Negate, 7))),Branch(None, Subtraction, Branch(None, Division, Leaf(None, 12), Leaf(None, 3)),Leaf(SquareRoot, 25)))
    Service.getNumberOfCommutativeOperators(tree) should be (2)
  }
}

