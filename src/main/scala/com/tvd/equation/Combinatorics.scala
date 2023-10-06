package com.tvd.equation

import com.tvd.equation.EquationConstant.UnaryOperator._
import scala.Predef._

object Combinatorics {

  /**
   * Generate all equations for the given list of arguments and list of operators.
   * Repetitive operators are allowed in the equation.
   *
   * @param arguments List of arguments that form the equation.
   * @param operators List of operators that can be used in the equation.
   * @param distinct If only distinct operators are allowed in equation TRUE otherwise FALSE.
   * @return Complete list of equations that can be formed with the given list of arguments and list of operators.
   */
  def equationPermutations(arguments: List[Int],
                           operators: List[String],
                           distinct: Boolean): List[List[(String, String, String, Int)]] = {
    if(distinct) equationPermutationsWithDistinctOperators(arguments, operators)
    else equationPermutationsWithRepetitiveOperators(arguments, operators)
  }

  /**
   * Generate all equations for the given list of arguments and list of operators.
   * Repetitive operators are allowed in the equation.
   *
   * @param arguments List of arguments that form the equation.
   * @param operators List of operators that can be used in the equation.
   * @return The list of branch equations
   */
  def equationPermutationsWithRepetitiveOperators(arguments: List[Int],
                                                  operators: List[String]): List[List[(String, String, String, Int)]] = {
    if(Validate.input(arguments, operators))
      argumentPermutations(arguments).map(operatorPermutations(operators, _, distinct = false)).flatten
    else null
  }

  /**
   * Generate all equations for the given list of arguments and list of operators.
   * Only distinct operators are allowed in the equation.
   *
   * @param arguments List of arguments that form the equation.
   * @param operators List of operators that can be used in the equation.
   * @return The list of branch equations
   */
  def equationPermutationsWithDistinctOperators(arguments: List[Int],
                                                operators: List[String]): List[List[(String, String, String, Int)]] = {
    if(Validate.input(arguments, operators))
      argumentPermutations(arguments).map(operatorPermutations(operators, _, distinct = true)).flatten
    else null
  }

  /**
   * Generate all permutations of p operators on p positions for a given set of p + 1 arguments.
   * Repetitive operators are allowed only if {distinct} argument is TRUE.
   *
   * @param operators The list of operators
   * @param equation The branch equation
   * @param distinct If true consider only distinct operators
   * @return The list of branch equations
   */
  def operatorPermutations(operators : List[String],
                           equation: List[(String, String, String, Int)],
                           distinct: Boolean) : List[List[(String, String, String, Int)]] = {
    if(distinct)
      operatorPermutationsUnique(operators, equation)
    else
      operatorPermutationsRepetitive(operators, equation)
  }

  /**
   * Generate all permutations of p operators on p positions for a given set of p + 1 arguments.
   * Repetitive operators are allowed (e.i. (+, +, +, ...).
   * Total number of permutations equal: n^^n (n to the power of n)
   *
   * @param operators The list of operators
   * @param equation The branch equation
   * @return The list of branch equations
   */
  def operatorPermutationsRepetitive(operators : List[String],
                                     equation: List[(String, String, String, Int)]): List[List[(String, String, String, Int)]] = {
    if(equation.size - operators.size != 1)
      throw new EquationException("The number of arguments must be equal with the (number of operators + 1)!")
    combinations(operators).map(k => {
      (None, None, None, equation(0)._4) ::
        (0 to (operators.size - 1)).map(x => (None, k(x), None, equation(x+1)._4)).toList
    })
  }

  /**
   * Generate all permutations of p operators on p positions for a given set of p + 1 arguments.
   * Only unique operators are allowed in the list.
   * Total number of permutations equal: n!
   *
   * @param operators The list of operators
   * @param equation The branch equation
   * @return The list of all branch equations
   */
  def operatorPermutationsUnique(operators : List[String],
                                 equation: List[(String, String, String, Int)]) : List[List[(String, String, String, Int)]] = {
    if(equation.size - operators.size != 1)
      throw new EquationException("The number of arguments must be equal with the (number of operators + 1)!")
    operators.permutations.toList.map(k => {
      (None, None, None, equation(0)._4) ::
        (0 to (operators.size - 1)).map(x => (None, k(x), None, equation(x+1)._4)).toList
    })
  }

  /**
   * Generate all permutations of n arguments.
   * Total number of permutations equal: n!
   *
   * @param arguments The list of arguments
   * @return The list of permutations of n arguments
   */
  def argumentPermutations(arguments: List[Int]): List[List[(String, String, String, Int)]] =
    arguments.permutations.toList.map(k =>
      (0 to (arguments.size - 1)).map(x =>
        (None,None,None,k(x))
      ).toList
    )

  /**
   * Basic generator of permutations for a list of n objects of type A as input.
   * Unique objects only in a combination.
   *
   * @param input The list of n objects
   * @tparam A The type of objects
   * @return The list of permutations of n object
   */
  def permutations[A](input : List[A]) : List[List[A]] = {
    if (input.isEmpty) List[List[A]](List[A]()) else {
      (List[List[A]]() /: input) {
        (accumulator, e) => accumulator :::
          permutations(input.filterNot({_ == e})).map(l => e :: l)
      }
    }
  }

  /**
   * Basic generator of combinations for a list of n objects on n positions as input.
   * Repetitive objects in a combination are allowed.
   *
   * @param operators The list of operators
   * @tparam A The type of operators
   * @return The list of operator combinations
   */
  def combinations[A](operators: List[A]) : List[List[A]] = {
    def comb(size: Int = operators.length) : List[List[A]] = {
      if (size == 0) List(List())
      else {
        for {
          x  <- operators.toList
          xs <- comb(size-1)
        } yield x :: xs
      }
    }
    comb()
  }
}
