package scalaalgebramodel 

object Q1 {
  // Attempt 1: O(2^x)
  def f1(x: Long): Long = x match {
    case 0 => 1
    case _ => f1(x - 1) + f1(x - 1)
  }
  
  // Attempt 2: O(x)
  def f2(x: Long): Long = x match {
    case 0 => 1
    case _ => 2 * f2(x - 1)
  }

  // Attempt 1 makes two recursive calls which each has a recursive depth of x. Therefore
  // the time complexity for f1 is O(2^n)
  // Attempt 2 makes a single call of depth x. Since we know the two calls are the same
  // we can simply double the result we get from a single call. This make the 
  // time complexity O(n)
}
