package com.tvd.equation

import scala.util.Random

import com.tvd.equation.EquationConstant.DifficultyRank._
import com.tvd.equation.EquationConstant.OperatorGroup._
import com.tvd.equation.EquationConstant.MaximumNumberOfIterations

object Generator {

  /**
   * Generate all valid equations for the given list of arguments, list of operators
   * and all parenthesis combinations and return a given value.
   *
   * @param arguments The list of arguments
   * @param operators The list of operators
   * @param distinct If true then consider only distinct operators
   * @param value The result the equations should provide
   * @return The list of (Tree) equations
   */
  def generateEquationsForValue(arguments: List[Int],
                                operators: List[String],
                                distinct: Boolean,
                                value: Int): List[List[Tree]] = {
    Combinatorics.equationPermutations(arguments, operators, distinct).map(x => {
      Catalan.generateAllTrees(x).filter(Operator.computeSafe(_) == (value, true))
    })
  }

  /**
   * Generate all equations for the given list of arguments, list of operators
   * and all parenthesis combinations.
   *
   * @param arguments The list of arguments
   * @param operators List of operators
   * @param distinct If true consider only distinct operators
   * @return The list of Tree equations
   */
  def generateEquations(arguments: List[Int],
                        operators: List[String],
                        distinct: Boolean): List[List[Tree]] =
    Combinatorics.equationPermutations(arguments, operators, distinct).map(x => Catalan.generateAllTrees(x))

  /**
   * Create a set of parameters that can be used to generate a list of equations that
   * can be solved with the given difficulty. The precision is the difference between
   * the difficulty score of the generated equations and the given difficulty argument.
   *
   * @param difficulty The equation difficulty as a number between 1 and 100
   * @param precision How close to the requested difficulty should the generated equations be
   * @return The equation request parameters that generate the given difficulty
   */
  def generateInputParametersForValidatedDifficulty(difficulty: Int, precision: Int): (Int, Int, Int, Int, Int) =
    difficulty match {
      case x if x < 0   =>
        generateInputParametersForDifficulty(0, precision, MaximumNumberOfIterations)
      case x if x > 100 =>
        generateInputParametersForDifficulty(100, precision, MaximumNumberOfIterations)
      case _ =>
        generateInputParametersForDifficulty(difficulty, precision, MaximumNumberOfIterations)
    }

  /**
   * Create a set of parameters that can be used to generate a list of equations that
   * can be solved with the given difficulty. A greater precision will require more
   * computation.
   * There is a limit in the number of loops that are used to generate equations given
   * by the value of the iteration argument.
   *
   * The set of parameters looks like this:
   * (pool of operators, number of operators, range of arguments, distinct operators, positive results)
   *
   * @param difficulty The equation difficulty as a number between 1 and 100
   * @param precision How close to the requested difficulty should the generated equations be
   * @param iteration How many attempts will be performed to generate equations with the given difficulty
   * @return The equation request parameters that generate the given difficulty
   */
  def generateInputParametersForDifficulty(difficulty: Int, precision: Int, iteration: Int): (Int, Int, Int, Int, Int) =  {
    // Randomly choose a difficulty combination
    //
    // 1. Pool of operators
    val index1 = Random.nextInt(PoolOfOperatorRanks.length)
    val poolOfOperators = PoolOfOperatorRanks(index1)._2

      // 2. Number of operators
    val maxNumberOfOperators = getNumberOfOperatorsFromPool(index1)
    val index2 = Random.nextInt(maxNumberOfOperators)
    val numberOfOperators = NumberOfOperatorRanks(index2)._2

      // 3. Range of arguments
    val index3 = Random.nextInt(RangeOfArgumentRanks.length)
    val rangeOfArguments = RangeOfArgumentRanks(index3)._2

      // 4. Distinct operators
    val index4 = Random.nextInt(DistinctOperatorRanks.length)
    val distinctOperators = DistinctOperatorRanks(index4)._2

      // 5. Positive results
    val index5 = Random.nextInt(PositiveOperatorRanks.length)
    val positiveOperators = PositiveOperatorRanks(index5)._2

    val score = poolOfOperators + numberOfOperators + rangeOfArguments + distinctOperators + positiveOperators
    val normalizedScore = score * 100 / MaximumAbsoluteRankingValue

    if (Math.abs(normalizedScore - difficulty) < precision)
    (index1, index2, index3, index4, index5)
    else {
      if(iteration < 0) generateInputParametersForDifficulty(difficulty, precision * 2, MaximumNumberOfIterations)
      else generateInputParametersForDifficulty(difficulty, precision, iteration - 1)
    }
  }

  /**
   * Get the number of operators from the pool of operators
   *
   * @param poolIndex The pool index
   * @return The number of operators
   */
  def getNumberOfOperatorsFromPool(poolIndex: Int): Int = poolIndex match {
    case 0 => OperatorGroup_1.length
    case 1 => OperatorGroup_2.length
    case 2 => OperatorGroup_3.length
    case 3 => OperatorGroup_4.length
    case 4 => OperatorGroup_5.length
    case 5 => OperatorGroup_6.length
    case _ => throw new EquationException("Pool of operators ["+poolIndex+"] doesn't exist!")
  }

  /**
   * Compute the value of difficulty given the set of equation generation parameters:
   * (pool of operators, number of operators, range of arguments, distinct operators, positive results)
   *
   * @param difficulty The set of equation generation parameters
   * @return The value of difficulty
   */
  def getValueOfDifficulty(difficulty: (Int, Int, Int, Int, Int)): Double = {
    val poolOfOperators   = PoolOfOperatorRanks(difficulty._1)._2
    val numberOfOperators = NumberOfOperatorRanks(difficulty._2)._2
    val rangeOfArguments  = RangeOfArgumentRanks(difficulty._3)._2
    val distinctOperators = DistinctOperatorRanks(difficulty._4)._2
    val positiveOperators = PositiveOperatorRanks(difficulty._5)._2

    val score = poolOfOperators + numberOfOperators + rangeOfArguments + distinctOperators + positiveOperators
    score * 100 / MaximumAbsoluteRankingValue
  }
}
