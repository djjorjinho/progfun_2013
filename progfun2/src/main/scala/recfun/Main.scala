package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    val matching = Map('(' -> ')', ')' -> '(')

    @tailrec def isBalanced(chars: List[Char], stack: List[Char]): Boolean = {

      if (chars.isEmpty) {
        stack.isEmpty
      } else if (chars.head.equals('(')) {
        isBalanced(chars.tail, stack ::: List(chars.head))
      } else if (chars.head.equals(')')) {
        !stack.isEmpty && matching(stack.last).equals(chars.head) &&
          isBalanced(chars.tail, stack.init)
      } else {
        isBalanced(chars.tail, stack)
      }

    }

    isBalanced(chars, List())

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
