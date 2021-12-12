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

object Homework :

  object `Boolean Operators` :

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

  object `Fermat Numbers` :

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

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = (n: Int) => {
      if (n <= 0) BigInt(0)
      var str = "1"

      if (n > 1) str = "11"

      var i = 3
      while (i <= n) {
        str += '$'
        var tp = 1
        var tmp = ""
        val mas = str.toCharArray
        for (j <- 1 until str.length) {
          if (mas(j) != mas(j - 1)) {
            tmp += tp + 0
            tmp += mas(j - 1)
            tp = 1
          } else {
            tp += 1
          }
        }
        str = tmp
        i += 1
      }
      BigInt(str)
    }

  end `Look-and-say Sequence`

end Homework