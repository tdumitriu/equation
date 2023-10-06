package com.tvd.equation

object EquationConstant {

  object UnaryOperator {
    val None = ""
    val Negate = "~"
    val Square = "\u00b2"
    val SquareRoot = "\u221a"
    val Factorial = "!"

    def list = List(None, Negate, Square, SquareRoot, Factorial)
  }

  object BinaryOperator {
    val Addition = "+"
    val Subtraction = "-"
    val Multiplication = "*"
    val Division = "/"
    val Power = "^"
    val Modulo = "%"

    def list = List(Addition, Subtraction, Multiplication, Division, Power, Modulo)
  }

  val CommutativeOperators = List(BinaryOperator.Addition, BinaryOperator.Multiplication)

  object Parenthesis {
    val LeftBracket = "("
    val RightBracket = ")"

    def list = List(LeftBracket, RightBracket)
  }

  object OperatorGroup {
    val og1 = 0
    val OperatorGroup_1 = List(BinaryOperator.Addition, BinaryOperator.Subtraction)

    val og2 = 1
    val OperatorGroup_2 = List(BinaryOperator.Addition, BinaryOperator.Subtraction,
                               BinaryOperator.Multiplication)

    val og3 = 2
    val OperatorGroup_3 = List(BinaryOperator.Addition, BinaryOperator.Subtraction,
                               BinaryOperator.Division)

    val og4 = 3
    val OperatorGroup_4 = List(BinaryOperator.Addition, BinaryOperator.Subtraction,
                               BinaryOperator.Multiplication, BinaryOperator.Division)

    val og5 = 4
    val OperatorGroup_5 = List(BinaryOperator.Addition, BinaryOperator.Subtraction,
                               BinaryOperator.Multiplication, BinaryOperator.Division,
                               BinaryOperator.Power)

    val og6 = 5
    val OperatorGroup_6 = List(BinaryOperator.Addition, BinaryOperator.Subtraction,
                               BinaryOperator.Multiplication, BinaryOperator.Division,
                               BinaryOperator.Power, BinaryOperator.Modulo)

    val CommutativeOperators = List(BinaryOperator.Addition.charAt(0), BinaryOperator.Multiplication.charAt(0))
  }

  val ArgumentMaximumValue = 100

  val MaximumNumberOfIterations = 1000

  val ListOfOperators = UnaryOperator.list ::: BinaryOperator.list

  object DifficultyRank {

    // POOL OF OPERATORS
    //
    //  Pool                 Rank
    // ---------------------------
    //  1 (+,-)          1
    //  2 (+,-,*)        2
    //  3 (+,-,/)        3
    //  4 (+,-,*,/)      4
    //  5 (+,-,*,/,^)    5
    //  6 (+,-,*,/,^,%)  7
    val PoolOfOperatorRanks = List((0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0), (4, 5.0), (5, 7.0))

    // NUMBER OF OPERATORS
    //
    //  #     Rank
    // ------------
    //  1       0
    //  2      20
    //  3      40
    //  4      60
    //  5      80
    //  6     100
    val NumberOfOperatorRanks = List((1, 0.0), (2, 20.0), (3, 40.0), (4, 60.0), (5, 80.0), (6, 100.0))

    // RANGE OF ARGUMENTS
    //
    // Range       Rank
    // -----------------
    // 0 - 10        1
    // 0 - 20        2
    // 0 - 50        3
    // 0 - 100       4
    val RangeOfArgumentRanks = List((10, 1.0), (20, 2.0), (50, 3.0), (100, 4.0))

    // DISTINCT OPERATORS
    //
    //  Distinct   Rank
    // -----------------
    //   False       1
    //   True       10
    val DistinctOperatorRanks = List((false, 1.0), (true, 10.0))

    // POSITIVE OPERATORS
    //
    //  Positive    Rank
    // ------------------
    //   False       1
    //   True        3
    val PositiveOperatorRanks = List((false, 1.0), (true, 3.0))

    val MaximumAbsoluteRankingValue = PoolOfOperatorRanks.maxBy(_._2)._2 + NumberOfOperatorRanks.maxBy(_._2)._2 +
                                      RangeOfArgumentRanks.maxBy(_._2)._2 + DistinctOperatorRanks.maxBy(_._2)._2 +
                                      PositiveOperatorRanks.maxBy(_._2)._2
  }
}
