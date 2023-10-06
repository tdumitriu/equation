package com.tvd.equation

import com.tvd.equation.EquationConstant.BinaryOperator._
import com.tvd.equation.Service.getChallenge

object ExampleTest {

  def main(args: Array[String]):Unit = {
//    example(args = List(1,2,3,4,5), ops = List(Addition, Subtraction, Multiplication, Division), distinct = true, result = 10, keep = 3)
    generate(pool = 3, opnmbr = 4, range = 11, distinct = true,  positive = true,  results = 10)
    // Generate all equations for
  }

  /**
   * Create a list of equations that meet the given requirements
   *
   * Example:
   *   pool     = 1    - Use only the following operators => (+, -, *)
   *   opnmbr   = 3    - Use 3 operators from the above list (so, all of them)
   *   range    = 16   - The maximum value an argument can take is 16
   *   distinct = true - Use distinct operators
   *   positive = true - Have only positive results (if possible)
   *   results  = 1    - Return only one equation, the best one
   *
   *   Note: The number of returned combinations is usually increased by the
   *         commutativity of some operators like addition and multiplication.
   *         So if commutative operators are present in the equation than the
   *         number of solutions can be safely divided by 2 to the power of
   *         the number of the commutative operators.
   *
   *         Examples:
   *            ((((6+10)/8)-1)*7) = 7 returns 4 solutions
   *
   *            Since it uses '+' and '*' we can get the actual number of solutions:
   *                 4 / (2 to the 2) = 1
   */
  def generate(pool: Int, opnmbr: Int, range: Int, distinct: Boolean, positive: Boolean, results: Int) = {
    val t1 = if(results == 1) "one equation" else s"max ${results} equations"
    val ts = if(results == 1) "s" else ""
    val t2 = if(distinct) "once" else "eventually with repetition"
    val t3 = if(positive) "only positive" else "positive and negative"
    val t4 = if(results == 1) "returns" else "return"
    val t5 = if(Service.operatorsPoolSize(pool) == opnmbr) "use" + ts + " all" else "use" + ts + " " + opnmbr

    println("\n==> Generate " + t1 + " that " + t5 + " of (" + Service.operatorsPoolList(pool) +
      ") operators taken " + t2 + " and " + (opnmbr + 1) +
      " arguments no greater than " + range + " that " + t4 + " " + t3 + " results.")

    // Generate the equations based on the requirements
    val equations: List[(Int, Tree)] = Service.generateEquations(pool, opnmbr, range, distinct,  positive, results)
    // Get the first result from list (there is anyway only one, since we requested one)
    //   The expected result is a tuple where:
    //    - the first element is the number of equations that can be created with
    //      the given list of operators and arguments that have the same result.
    //    - and the second element is the equation returned as a Tree object
    // Print out the result
    equations.foreach { case (solutions, equation) =>
      val sol = getChallenge(equation)
      println("[" + sol.arguments + "], [" + sol.operators + "], [" + sol.solution + "] " + "\t\t\t\t\t\t\t[" + solutions + "] solutions for " + equation.text + " = " + equation.value )
    }

    println("\n\n\n\n\n\n\n\n\n\n\n\n")

    equations.foreach { case (solutions, equation) =>
      val sol = getChallenge(equation)
      println("[" + solutions + "] [" + sol.arguments.mkString(", ") + "]  [" + sol.operators.mkString(", ") + "] = [" + sol.solution + "]")
    }
  }


  /**
   * Generated a list of equations that satisfy the given requirements
   *
   * Example:
   *     args = List(1,5,9,13) - Use the arguments 1, 5, 9, 13
   *     ops = List(+,-,*)     - Use the operators +, -, *
   *     distinct = true       - Use all and every operator only once
   *     result = 5            - The result of equations
   *     keep = 2              - Return only two equations
   *
   * @param args List of arguments
   * @param ops List of operators
   * @param distinct If true return only equations with distinct operators (no repeats)
   * @param result The result of the equations
   * @param keep Number of best equations to keep. The best equations are the equations
   *             that have the smallest number of variations that return a certain result.
   * @return The list of equations
   */
  def example(args: List[Int], ops: List[String], distinct: Boolean, result: Int, keep: Int) = {
    val t1 = if(keep == 1) "one equation" else "a list of " + keep + " equations"
    val t2 = if(distinct) "distinct" else "eventually the repeated"
    val t3 = if(keep == 1) "equation is" else "list of " + keep + " equations is composed of the following"

    println("\n==> Generate " + t1 + " with the arguments [" + args.mkString(",") + "] and " +
      t2 + " operators [" + ops.mkString(",") + "] that have the result equal with [" + result + "]")

    val equations = Service.getEquationSolution(args, ops, distinct, result)
    println("Total solutions = [" + equations.size + "].\nThe requested " + t3 + ":")
    equations.take(keep).map(equation => println("\t" + equation.text + " = " + equation.value))
  }
}