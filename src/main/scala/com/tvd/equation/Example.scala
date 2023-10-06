package com.tvd.equation

import com.tvd.equation.EquationConstant.BinaryOperator._

object Example {

  def main(args: Array[String]) {

    println("-------------------------------------------------------------------------------------")
    println("Question 1: Given the equation in string format give me the tree and the solution:")
    println("/solution?eq=((7+(2/(5-3)))*4)")
    println("{32}")
    println("{Error: Invalid character [?] found in equation!}")
    println("-------------------------------------------------------------------------------------")
    // TODO: extend the parsing to unary operators
    example1("((3+5)/(3-1))")
//    example1("(5-!3)")
    example1("(5-(-3))")
    example1("(5-(+3))")
    example1("((1-2)*(3+4))")
    example1("((2^8)-3)")
    example1("((7+(24/(5-2)))*4)")
    example1("(((7+(24/(5-3)))*4)-((15/(4-1))*2))")
    example1("((((3*2)-7)*((6/3)+7))+((12/3)-2))")

    example2("((7+(24/(5-2)))*4)")
    example2("((7+(24/(5-5)))*4)")
    example2("((((3*2)-7)*((6/3)+7))+((12/3)-2))")

    println("-------------------------------------------------------------------------------------")
    println("Question 2: Given the difficulty give me the equations and the normalized difficulty:")
    println("/generate/questions?pool=3&opnmbr=4&range=16&distinct=true&positive=true&results=1")
    println("{args:{'1','2','10','15'}, ops:{'-','*','+'}, solution:{2}, difficulty:{41}}")
    println("Error: No equations can be generated.")
    println("-------------------------------------------------------------------------------------")
    // TODO: Make sure that the 'difficulty' can be returned
    example3(pool = 1, opnmbr = 3, range = 16, distinct = true,  positive = true,  results = 1)
    example3(pool = 1, opnmbr = 2, range = 12, distinct = true,  positive = true,  results = 1)
    example3(pool = 3, opnmbr = 4, range = 10, distinct = true,  positive = true,  results = 4)
    example3(pool = 3, opnmbr = 3, range = 20, distinct = false, positive = false, results = 4)
    example3(pool = 2, opnmbr = 3, range = 16, distinct = true,  positive = true,  results = 2)
    example3(pool = 3, opnmbr = 4, range = 10, distinct = true,  positive = true,  results = 2)

    println("-------------------------------------------------------------------------------------")
    println("Question 3: Given the input arguments and operators give me the best three generated equations:")
    println("/generate/solutions?args=8,18,21,22&ops=+,-,*&distinct=true&positive=true")
    println("{equations [{equation:{((8*(21-22))+18)}, result:{10}, variations:{1}}, {equation:{((8*18)-(21+22))}, result:{101}, variations:{1}}]}")
    println("Error: No equations can be generated.")
    println("Error: No positive equations can be generated.")
    println("-------------------------------------------------------------------------------------")
    example4(args = List(8,18,21,22), ops = List(Addition, Subtraction, Multiplication), distinct = true, positive = true, keep = 3)
    example4(args = List(3,4,9,13), ops = List(Addition, Subtraction, Division), distinct = true, positive = true, keep = 1)
    example4(args = List(1,3,7,13, 17), ops = List(Addition, Subtraction, Multiplication, Division), distinct = true, positive = false, keep = 2)

    println("-------------------------------------------------------------------------------------")
    println("Question 4: Given the input arguments and operators give me two equations for the result and the total number of solutions:")
    println("/resolve?arguments=3,7,8,9,11&operators=+,-,*,/&distinct=true&result=5&keep=2")
    println("{equations [{equation:{((7+(8*(9-3)))/11)}, result:{5}}, {equation:{((7+((9-3)*8))/11)}, result:{5}}], variations:{24}}")
    println("Error: Solution cannot be found.")
    println("-------------------------------------------------------------------------------------")
    example5(args = List(3,7,8,9,11), ops = List(Addition, Subtraction, Multiplication, Division), distinct = true, result = 5, keep = 2)
    example5(args = List(1,3,7,13,17), ops = List(Addition, Subtraction, Multiplication, Division), distinct = true, result = 37, keep = 3)
    example5(args = List(3,5,7,9), ops = List(Addition, Subtraction, Multiplication), distinct = true, result = 11, keep = 2)
  }

  /**
   * Print out a string equation after transforming it in a Tree object
   */
  def example1(equation: String) = {
    println("\n==> Convert " + equation + " to a Tree object and then print it out")
    // Firstly, transform the string equation to a Tree object
    val tree: Tree = Service.generateTree(equation.toList)
    // Secondly, get the equation text from the Tree object (obviously we could have used the given string :))
    val equationBody = tree.text
    // Thirdly, solve the equation and get the result
    val equationResult = Operator.compute(tree)
    // Print out the results
    println("\t" + equationBody + " = " + equationResult)
  }

  /**
   * Equation validation makes sure that the generated (or given)
   * equation meets a set of rules or better said it does not
   * satisfy the following exceptions:
   *
   *   1. Arithmetic operations exceeding the integer maximum value
   *   2. Division not returning an integer
   *   3. Square root not returning an integer
   8   4. Zero to the power of zero
   *   5. Modulo of zero
   *   6. Factorial of a negative number
   *   7. Root of a negative number
   *
   * An invalid equation computed safely will return zero as a result.
   * An invalid equation computed unsafely will throw an exception.
   *
   */
  def example2(equation: String) = {
    println("\n==> Compute safely the equation " + equation)
    // Transform the string equation to a Tree object
    val tree: Tree = Service.generateTree(equation.toList)
    // Resolve the equation safely
    val (result, isSafe) = Operator.computeSafe(tree)
    // Print out the results
    println("\tValidation = [" + isSafe + "] for " + equation + " = " + result)
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
  def example3(pool: Int, opnmbr: Int, range: Int, distinct: Boolean,  positive: Boolean, results: Int) = {
    val t1 = if(results == 1) "one equation" else results + " equations"
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
    equations.map { case (solutions, equation) =>
      println("\t[" + solutions + "] solutions for " + equation.text + " = " + equation.value)
    }
  }

  /**
   * Generated a list of equations that satisfy the given requirements
   *
   * Example:
   *     args = List(1,5,9,13) - Use the arguments 1, 5, 9, 13
   *     ops = List(+,-,*)     - Use the operators +, -, *
   *     distinct = true       - Use all and every operator only once
   *     positive = true       - Return only the equations that have positive results.
   *     keep = 2              - Return only two equations
   *
   * @param args List of arguments
   * @param ops List of operators
   * @param distinct If true return only equations with distinct operators (no repeats)
   * @param positive If true return only equations that have positive results
   * @param keep Number of best equations to keep. The best equations are the equations
   *             that have the smallest number of variations that return a certain result.
   * @return The list of equations
   */
  def example4(args: List[Int], ops: List[String], distinct: Boolean, positive: Boolean, keep: Int) = {
    val t1 = if(keep == 1) "one equation" else "a list of " + keep + " equations"
    val t2 = if(distinct) "distinct" else "eventually the repeated"
    val t3 = if(positive) "only positive" else "positive and negative"
    println("\n==> Generate " + t1 + " with the arguments [" + args.mkString(",") + "] and " +
      t2 + " operators [" + ops.mkString(",") + "] and with " + t3 + " results.")

    Service.getResultEquationMap(Generator.generateEquations(args, ops, distinct)).
      filterKeys(x => { if (positive) x >= 0 else true }).toSeq.sortBy(_._2.size).take(keep).map {
        case (result: Int, equations: List[Tree]) =>
          println(equations.head.text + " = " + result + " with [" +
            (equations.size / Math.pow(2.0, Service.getNumberOfCommutativeOperators(equations.head))).toInt +
            "] variation(s)")}
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
  def example5(args: List[Int], ops: List[String], distinct: Boolean, result: Int, keep: Int) = {
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