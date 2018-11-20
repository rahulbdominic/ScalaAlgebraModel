package scalaalgebramodel 

object Q1 {
  // Attempt 1: O(2^x)
  def f1(x: Long): Long = x match {
    case 0 => 1
    case _ => f1(x - 1) + f1(x - 1)
  }
  
  // Attempt 1: O(x)
  def f2(x: Long): Long = x match {
    case 0 => 1
    case _ => 2 * f2(x - 1)
  }
}
