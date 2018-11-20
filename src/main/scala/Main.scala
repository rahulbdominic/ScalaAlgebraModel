package scalaalgebramodel 

import Q1.f1
import Q1.f2

object Main extends App {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
  
  override def main(args: Array[String]): Unit = {
    // Profile Q1
    //println(time { f2(30) })
    //println(time { f1(30) })
  }
}