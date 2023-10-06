package com.tvd.equation

/**
 * The number of binary trees with n nodes is given by the Catalan numbers,
 * which have many interesting properties.
 * The nth Catalan number is determined by the formula (2n)! / (n+1)!n!
 *
 * The number of binary trees of a given size is:
 * 0	           1
 * 1	           1
 * 2	           2
 * 3             5
 * 4	          14
 * 5            42
 * 6           132
 * 7           429
 * 8	       1,430
 * 9         4,862
 * 10       16,796
 * 12  	   208,012
 * 16	  35,357,670
 */
object Catalan {

  val Empty = ""
  val Zero = 0

  def generateAllTrees(size: Int): List[Tree] = size match {
    case n if n  < 1 =>
      List(Leaf(Empty, Zero))
    case n if n == 1 =>
      List(Branch(Empty, Empty, Leaf(Empty, Zero), Leaf(Empty, Zero)))
    case n if n > 1 =>
      var allTrees = List[Tree]()
      for(i <- 0 to n-1) {
        val leftTree = generateAllTrees(i)
        val rightTree = generateAllTrees(n-i-1)
        allTrees = allTrees ::: leftTree.map(l => rightTree.map(r => Branch(Empty, Empty, l, r))).flatten
      }
      allTrees
  }

  def traverseTree(tree: Tree, args: List[(String, String, String, Int)]): Tree = {
    var pos_arg = - 1
    var pos_op = 0
    def traverseTree(tree: Tree, args: List[(String, String, String, Int)], arg: List[(String, String, String, Int)]): Tree = {
      tree match {
        case Leaf(u,x) =>
          pos_arg = pos_arg + 1
          Leaf(arg(pos_arg)._3, arg(pos_arg)._4)
        case Branch(u, b, left, right) =>
          pos_op = pos_op + 1
          Branch(arg(pos_op)._1, arg(pos_op)._2, traverseTree(left, args.tail, arg), traverseTree(right, args.tail, arg))
      }
    }
    traverseTree(tree, args, args)
  }

  def generateAllTrees(input: List[(String, String, String, Int)]): List[Tree] =
    generateAllTrees(input.size - 1).map(tree => traverseTree(tree, input))

  def factorial(n: BigInt) = n match {
    case x if x < 0 =>
      throw new EquationException("Factorial of a negative number is undefined!")
    case x if x > 32 =>
      throw new EquationException("This result is irrelevantly big for the current application!")
    case _ =>
      BigInt(1).to(n).foldLeft(BigInt(1))(_ * _)
  }

  def catalanNumber(n: BigInt) = factorial(2 * n) / (factorial(n + 1) * factorial(n))
}
