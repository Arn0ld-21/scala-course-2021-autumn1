package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec

/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean =
      if (b) false
      else true

    def and(left: Boolean, right: Boolean): Boolean =
      if (!left) false
      else if (!right) false
      else true

    def or(left: Boolean, right: Boolean): Boolean =
      if (left) true
      else if (right) true
      else false

  end `Boolean Operators`

  object `Fermat Numbers`:

    val multiplication: (BigInt, BigInt) => BigInt = (left, right) => {
      @tailrec
      def inner(left: BigInt, right: BigInt, ant: BigInt): BigInt = {
        if (left == 0) ant
        else inner(left - 1, right, ant + right)
      }

      inner(left, right, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (left, right) => {
      @tailrec
      def inner(left: BigInt, right: BigInt, ant: BigInt): BigInt = {
        if (right == 0) ant
        else inner(left, right - 1, multiplication(ant, left))
      }

      inner(left, right, 1)
    }

    val fermatNumber: Int => BigInt =
      i => power(2, power(2, i)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence`:
    val lookAndSaySequenceElement: Int => BigInt = n => {

      @tailrec
      def searchN(sp: List[Int], i: Int, result: String): BigInt = {
        if i == sp.length
        then BigInt(result)
        else {
          val amount = countI(sp, i, sp(i), 0)
          val newI = i + amount
          val newResult = result.concat(amount.toString()).concat(sp(i).toString)
          searchN(sp, newI, newResult)
        }
      }

      @tailrec
      def countI(sp: List[Int], i: Int, item: Int, amount: Int): Int = {
        if i == sp.length || sp(i) != item
        then amount
        else
          countI(sp, i + 1, item, amount + 1)
      }

      @tailrec
      def searchNEl(n: Int, enumer: Int, result: BigInt): BigInt = {
        if enumer == n
        then result
        else {
          val num = result.toString().map(_.asDigit).toList
          val newElement = searchN(num, 0, "")
          searchNEl(n, enumer + 1, newElement)
        }
      }

      if n < 0
      then throw new IllegalArgumentException("Illegal argument with negative value: " + n)
      else
        searchNEl(n, 0, 1)

    }

  end `Look-and-say Sequence`

end Homework