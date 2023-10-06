package com.tvd.equation

import com.tvd.equation.EquationConstant.DifficultyRank._
import com.tvd.equation.EquationConstant.OperatorGroup._
import com.tvd.equation.EquationConstant.UnaryOperator._

import scala.util.Random
import scala.util.matching.Regex

object Service {

  /**
   * Generate a list of equations with the given difficulty.
   *
   * @param difficulty Value that determines the difficulty of the equations.
   *
   *      Output:
   *                   A. Equations that resolve to "cute" numbers
   *                   -------------------------------------------
   *                   1,2,3,4,5,6,7,8,9,10,11,12,15,16,20,25,50,100
   *
   *                   B. Number of equations for solution
   *                   -----------------------------------
   *                   1,2,4,6,8,12,16,20,24,32,48,64
   *
   *                   C. Statistical summary value
   *                   ----------------------------
   *                   2.0, 3.0, 4.0, 8.0
   *
   * @return List of Tuple formatted as (number_of_equations_with_identical_result, equation)
   */
  def getEquationsByDifficulty(difficulty: Int): List[(Int, Tree)] = {
    // Get the settings for given difficulty
    val (poolOfOperators, numberOfOperators, rangeOfArguments, uniqueOperators, positiveOperators) =
      Generator.generateInputParametersForValidatedDifficulty(difficulty, 3)
    //
    // 1. Pool of operators
    val poolOperators = PoolOfOperatorRanks(poolOfOperators)._1
    // 2. Number of operators
    val numberOfOperatorsFromPool = NumberOfOperatorRanks(numberOfOperators)._1
    // 3. Range of arguments
    val maxArgumentValue = RangeOfArgumentRanks(rangeOfArguments)._1
    // 4. Distinct operators
    val distinctOperators = DistinctOperatorRanks(uniqueOperators)._1
    // 5. Positive results
    val onlyPositiveResults = PositiveOperatorRanks(positiveOperators)._1

    if(poolOperators == 0)  println("A. Pool of operators = ( + , - )")
    if(poolOperators == 1)  println("A. Pool of operators = ( + , - , * )")
    if(poolOperators == 2)  println("A. Pool of operators = ( + , - , / )")
    if(poolOperators == 3)  println("A. Pool of operators = ( + , - , * , / )")
    if(poolOperators == 4)  println("A. Pool of operators = ( + , - , * , / , ^ )")
    if(poolOperators == 5)  println("A. Pool of operators = ( + , - , * , / , ^ , % )")
    println("Number of operators from pool = " + numberOfOperatorsFromPool)
    println("Maximum value of arguments = " + maxArgumentValue)
    println("Distinct operators = " + distinctOperators)
    println("Positive operators = " + onlyPositiveResults)
    // Number of results that are considered for analysis
    val numberOfResults = 9
    //
    // Generate the equations
    //
    generateEquations(poolOperators, numberOfOperatorsFromPool, maxArgumentValue,
      distinctOperators, onlyPositiveResults, numberOfResults)
  }

  /**
   * Generate equations based on the given arguments:
   *
   *                   A. Pool of operators
   *                   --------------------
   *                   0 = (+ , -)
   *                   1 = (+, -, *)
   *                   2 = (+, -, /)
   *                   3 = (+, -, *, /)
   *                   4 = (+, -, *, /, ^)
   *                   5 = (+, -, *, /, ^, %)
   *
   *                   B. Number of operators
   *                   ----------------------
   *                   1, 2, 3, 4, 5, 6
   *
   *                       Implies:
   *                       B.1 Number of arguments
   *                       ----------------------
   *                       2, 3, 4, 5, 6, 7
   *
   *                   C. Range of arguments
   *                   ---------------------
   *                   1=10, 2=20, 3=50, 4=100
   *
   *                   D. Operator distinct
   *                   --------------------
   *                   T - True  (only distinct operators allowed)
   *                   F - False (repetitive operators allowed)
   *
   *                   E. Positive equations
   *                   ---------------------
   *                   T - True  (Only equations that have positive values)
   *                   F - False (Equations with integer values)
   *
   *                   F. Number of results
   *                   ---------------------
   *                   1, 2, 3, ...
   *
   *
   *      Output:
   *                   A. Equations that resolve to "cute" numbers
   *                   -------------------------------------------
   *                   1,2,3,4,5,6,7,8,9,10,11,12,15,16,20,25,50,100
   *
   *                   B. Number of equations for solution
   *                   -----------------------------------
   *                   1,2,4,6,8,12,16,20,24,32,48,64
   *
   *                   C. Statistical summary value
   *                   ----------------------------
   *                   2.0, 3.0, 4.0, 8.0
   *
   * @param pool The pool of operators.
   * @param opnmbr Number of operators extracted randomly from the pool.
   * @param range Maximum value of the arguments that are randomly generated.
   * @param distinct If TRUE only distinct operators allowed, otherwise repetitive operators allowed.
   * @param positive If TRUE then only positive solutions allowed, if FALSE then negative solutions also allowed.
   * @param results Number of generated solutions.
   * @return A list of equations that satisfy the input arguments.
   */
  def generateEquations(pool: Int,
                        opnmbr: Int,
                        range: Int,
                        distinct: Boolean,
                        positive: Boolean,
                        results: Int): List[(Int, Tree)] = {
    //
    // Pre activity: Generate initial input items
    val operators = generateRandomOperators(pool, opnmbr)
    val numberOfArguments = operators.size + 1
    val arguments = generateRandomArguments(numberOfArguments, range)
    //
    // Main activity: Generate equations
    val equations: Map[Int, List[Tree]] =
      Service.getResultEquationMap(Generator.generateEquations(arguments, operators, distinct))
    //
    // Post activity: Get the summary of generated equations
    val summary: Map[Int, List[Int]] =
      Service.getEquationResultSummary(equations)
    //
    // Post activity: Filter summary for best results
    val finalList: List[(Int, Int)] =
      Service.getEquationWinners(summary, results, positive)
    //
    // Presentation activity: Prepare final list
    if(finalList.nonEmpty)
      finalList.map(x => {
        val equation = Service.getEquationsByValue(equations, x._2).head
        val commOp = getNumberOfCommutativeOperators(equation)
        val solutions = x._1 / Math.pow(2.0, commOp).toInt
        (solutions, equation)
      }) else List()
  }

  def getChallenge(tree: Tree): Challenge = {
    // (((10+2)/6)-1) = 1
    val equation = tree.text
    val solution = tree.value

    val xText = equation.split("=").toList
    val left = xText.head

    val numbers: Regex = """\d+""".r
    val arguments: List[Int] = numbers.findAllIn(left).toList.map(_.toInt).sorted

    val ops: Regex = """\+|-|\*|\/""".r
    val operators: List[String] = ops.findAllIn(left).toList.sorted

    Challenge(arguments, operators, solution)
  }


  /**
   * Get the the equation that resolves to the given value.
   *
   * @param arguments The arguments of the equation.
   * @param operators The operators of the equation.
   * @param distinctOperators If <code>true</code> then only distinct operators are allowed in equation.
   *                          If <code>false</code> then any combination of operators are allowed.
   * @param solution The value of the equation.
   * @return The equation that resolves to the given value.
   */
  def getEquationSolution(arguments: List[Int],
                          operators: List[String],
                          distinctOperators: Boolean,
                          solution: Int): List[Tree] = {
    val equations: Map[Int, List[Tree]] =
      Service.getResultEquationMap(Generator.generateEquations(arguments, operators, distinctOperators))
    if(equations.contains(solution)) {
      equations.get(solution).get
    } else List[Tree]()
  }

  /**
   * Get the sorted summary of equation generation results in terms of:
   *  List[{number_of_equations_with_identical_result}, {equation_result_value}]
   *
   * @param summary the summary result of equation generations.
   * @param max the number of returned summary results
   * @param onlyPositives If TRUE then return the smallest positive value.
   *                      If there are no positive values then return the highest negative value.
   *                      If FALSE then return the closest to zero value.
   * @return list of tuple that represents the number of equations and their values.
   */
  def getEquationWinners(summary: Map[Int, List[Int]],
                         max: Int, onlyPositives: Boolean): List[(Int, Int)] = {
    summary.toList.sortBy(_._1).take(max).map { x =>
      if(onlyPositives) {
        if(x._2.toList.filter(_ >= 0).nonEmpty) {
          (x._1, x._2.toList.filter(_ >= 0).sorted.head)
        } else {
          (x._1, x._2.toList.sorted.last)
        }
      } else {
        (x._1, x._2.toList.sortWith(Math.abs(_) <= Math.abs(_)).head)
      }
    }
  }

  /**
   * Get the equation that has the value equal to 'key'.
   *
   * @param equations A map of equations. Key is the result of equation,
   *                  value is the list of equations satissfying the key.
   *
   * @param key Value that the equation matches
   * @return
   */
  def getEquationsByValue(equations: Map[Int, List[Tree]], key: Int): List[Tree] = {
    if(equations.contains(key)) equations.get(key).get
    else Nil
  }

  /**
   * Generate random n arguments with values from a pool of 1 to p.
   *
   * @param n number of generated arguments
   * @param p the maximum potential value. The minimum potential value is 1.
   * @return list of n random arguments
   */
  def generateRandomArguments(n: Int, p: Int): List[Int] = {
    var r = List[Int]()
    do {
      r = Seq.fill(n)(Random.nextInt(p)).toList
    } while(r.distinct.size != r.size)
    r.map(_ + 1)
  }

  /**
   * Generate a random position within a list of size 'p'.
   *
   * @param p the max value.
   * @return a random number less than 'p'
   */
  def getRandomPosition(p: Int): Int = {
    if(p < 0)
      throw new EquationException("Range of random value should be positive!")
    if(p == 0)
      0
    else
      Random.nextInt(p)
  }

  /**
   * Compute the number of combinations that can be made with n operators from a pool pf p operators
   *
   * @param p The number o operators from the pool of operators
   * @param n The number o operators from a combinations
   * @return The number of combinations
   */
  def combinationOf(p:Int, n: Int): Int = {
    // 4C2 = 4! / (4 - 2)!2!
    if(p < 1)
      throw new EquationException("Size of combination pool should be greater than 0!")
    if(n < 0)
      throw new EquationException("The group size of combinations should be positive!")
    if(p < n)
      throw new EquationException("Size of combination pool should be greater than the group size of combinations!")
    Operator.factorial(p) / (Operator.factorial(p - n) * Operator.factorial(n))
  }

  /**
   * Generate the partial combinations of n operators from 'operatorPool'
   *
   * @param operatorPool The pool of operators
   * @param n Number o operators taken in combinations
   * @return A list of combinations of n operators from the given pool of operators
   */
  def generatePartialCombinations(operatorPool: List[String], n: Int): List[String] = {
    var part = n
    if(n > operatorPool.size) part = operatorPool.size
    val pos = combinationOf(operatorPool.size, part)
    operatorPool.combinations(part).toList(getRandomPosition(pos))
  }

  /**
   * Generate a list of operators.
   *
   * @param pool If OperatorGroup_1 (og 0) then 'Addition', 'Subtraction'
   *             If OperatorGroup_2 (og 1) then 'Addition', 'Subtraction', 'Multiplication'
   *             If OperatorGroup_3 (og 2) then 'Addition', 'Subtraction', 'Division'
   *             If OperatorGroup_4 (og 3) then 'Addition', 'Subtraction', 'Multiplication', 'Division'
   *             If OperatorGroup_5 (og 4) then 'Addition', 'Subtraction', 'Multiplication', 'Division', 'Power'
   *             If OperatorGroup_6 (og 5) then 'Addition', 'Subtraction', 'Multiplication', 'Division', 'Power', 'Modulo'
   * @param n The number of operators that are taken from the pool.
   * @return A random list of operators.
   */
  def generateRandomOperators(pool: Int, n: Int): List[String] = pool match {
    case 0 => generatePartialCombinations(OperatorGroup_1, n)
    case 1 => generatePartialCombinations(OperatorGroup_2, n)
    case 2 => generatePartialCombinations(OperatorGroup_3, n)
    case 3 => generatePartialCombinations(OperatorGroup_4, n)
    case 4 => generatePartialCombinations(OperatorGroup_5, n)
    case 5 => generatePartialCombinations(OperatorGroup_6, n)
    case _ => List()
  }

  /**
   * Get the statistics of a summary record.
   *
   * @param element The tuple of 'number of solutions' and 'list of equation values'
   * @return Tuple of 7 statistical values (number of solutions, number of values for solutions, minimum solutions value,
   *         maximum solutions value, average solutions value, median solutions value,
   *         positive closest solutions value to zero (if only negatives are available then the greatest negative))
   */
  def getSummaryElementStatistics(element: (Int, List[Int])): (Int, Int, Int, Int, Double, Int, Int) = {
    val number = element._1
    val values = element._2
    ( // 1. number of solutions
      number,
      // 2. number of values corresponding to a certain number of solutions
      values.length,
      // 3. minimum value
      values.min,
      // 4. maximum value
      values.max,
      // 5. average value
      values.reduceLeft(_ + _) / values.length.asInstanceOf[Double],
      // 6. median value
      values.sorted.toList(values.length / 2),
      // 7. positive closest value to zero
      // (if only negatives are available then the greatest negative)
      { if(values.filterNot(x => x < 0).isEmpty) {
          values.sorted.last
        } else {
          values.filterNot(x => x < 0).sorted.head
        }
      }
    )
  }

  def getStandardDeviation(xs: List[Double], average: Double): Double = xs match {
    case Nil => 0.0
    case ys => math.sqrt((0.0 /: ys) {
      (a,e) => a + math.pow(e - average, 2.0)
    } / xs.size)
  }

  def getAverage(xs: List[Double]): Double = xs match {
    case Nil => 0.0
    case ys => ys.reduceLeft(_ + _) / ys.size.toDouble
  }

  /**
   * Get the summary of the list of generated equations
   *
   * @param equations The map of generated equations [key = number of solutions, value = list of equations]
   * @return A map of results summary [key = number of solutions, value = list of equation results]
   */
  def getEquationResultSummary(equations: Map[Int, List[Tree]]): Map[Int, List[Int]] = {
    equations.toSeq.sortBy(_._1).toMap.map(x =>
      (x._1, x._2.size)
    ).groupBy(_._2).map{
      case (key, value) => (key, value.unzip._1.toList)
    }
  }

  /**
   * Get the distribution of the number of solutions
   *
   * @param summary The equation as input
   * @return
   */
  def getSummaryDistribution(summary: Map[Int, List[Int]]): Float = {
    summary.unzip._2.map(x => x.size).foldLeft(0)(_ + _).toFloat / summary.size
  }

  /**
   * Get the median number of solutions
   *
   * @param summary The equation as input
   * @return
   */
  def getSummaryMedian(summary: Map[Int, List[Int]]): Int =  {
    val l = summary.toList.sortBy(_._1)
    l(summary.size / 2)._1
  }

  /**
   * Get the average number of solutions
   *
   * @param summary The equation as input
   * @return
   */
  def getSummaryAverage(summary: Map[Int, List[Int]]): Int =  {
    summary.toList.map(_._1).sum / summary.size
  }

  /**
   * Get the minimum number of solutions
   *
   * @param summary The equation as input
   * @return
   */
  def getSummaryMinimum(summary: Map[Int, List[Int]]): Int = {
    summary.toList.sortBy(_._1).head._1
  }

  /**
   * Get the maximum number of solutions
   *
   * @param summary The equation as input
   * @return
   */
  def getSummaryMaximum(summary: Map[Int, List[Int]]): Int = {
    summary.toList.sortBy(_._1).last._1
  }

  /**
   * Convert the list of equation to a valid map [key = equation's value, value = list of equations]
   * Note: valid map includes only the equations that are valid.
   *
   * @param equations The equation as input
   * @return
   */
  def getResultEquationMap(equations: List[List[Tree]]): Map[Int, List[Tree]] = {
    equations.flatten.filter(Operator.computeSafe(_)._2).groupBy(Operator.compute)
  }

  /**
   * Convert the list of equations to a list of valid Tuple of two (equation's value, equation)
   * Note: valid list includes only the equations that are valid.
   *
   * @param equations The equation as input
   * @return
   */
  def getResultEquationPairs(equations: List[List[Tree]]): List[(Int, Tree)] = {
    equations.flatten.filter(Operator.computeSafe(_)._2).map( x => (Operator.compute(x), x) )
  }

  /**
   * Generate a Tree object from a string equation
   *
   *
   * @param value List of characters composing the equation
   * @return Tree object
   */
  // TODO: unary operation are not included. Ex: (3 + (-3)) = 0
  def generateTree(value: List[Char]): Tree = value.head match {
    case '(' =>
      val r = Validate.insertDelimiters(value).mkString.split('|')
      Branch(None, r(1), generateTree(r(0).toList), generateTree(r(2).toList))
    case x if x.isDigit =>
      Leaf(None, value.mkString.toInt)
    case _ =>
      throw new EquationException("Invalid character ["+value.head+"] in equation!")
  }

  def getNumberOfCommutativeOperators(equation: String): Int =
    equation.count(CommutativeOperators.contains(_))

  def getNumberOfCommutativeOperators(tree: Tree): Int =
    getNumberOfCommutativeOperators(tree.text)

  def operatorsPoolList(pool: Int): String =
    pool match {
      case 0 => OperatorGroup_1.mkString(",")
      case 1 => OperatorGroup_2.mkString(",")
      case 2 => OperatorGroup_3.mkString(",")
      case 3 => OperatorGroup_4.mkString(",")
      case 4 => OperatorGroup_5.mkString(",")
      case 5 => OperatorGroup_6.mkString(",")
      case _ => ""
    }

  def operatorsPoolSize(pool: Int): Int =
    pool match {
      case 0 => OperatorGroup_1.size
      case 1 => OperatorGroup_2.size
      case 2 => OperatorGroup_3.size
      case 3 => OperatorGroup_4.size
      case 4 => OperatorGroup_5.size
      case 5 => OperatorGroup_6.size
      case _ => 0
    }
}