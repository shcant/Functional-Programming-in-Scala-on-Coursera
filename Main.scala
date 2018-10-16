package recfun

object Main {
  def main(args: Array[String]) {
    import scala.util.control.Breaks._
    breakable {
      while(true) break
    }
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
    def pascal(c: Int, r: Int): Int = if (r < 0 || c < 0) 0 else if (r == 0 && c == 0) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceRecursive(chars: List[Char], cnt: Int) : Boolean = {
        if (chars.isEmpty) cnt == 0
        else if (chars.head == '(' && cnt >= 0)  balanceRecursive(chars.tail, cnt + 1)
        else if (chars.head == ')') balanceRecursive(chars.tail, cnt - 1)
        else balanceRecursive(chars.tail, cnt)
      }
      balanceRecursive(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeRecursive(money: Int, coins: List[Int]): Int = {
        if (money < 0 || coins.isEmpty) 0
        else if (money == 0) 1
        else countChangeRecursive(money, coins.tail) + countChangeRecursive(money - coins.head, coins)
      }
      if (money == 0) 0 else countChangeRecursive(money, coins)
    }
  }
