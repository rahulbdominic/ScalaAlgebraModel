package scalaalgebramodel 

import collection.mutable.Stack
import org.scalatest._

class BooleanExpressionTest extends FlatSpec with Matchers {
  "toJson" should "return successful result for atomic BooleanExpression" in {
     val expression = And(True, False)
     val expected = """
      {"opType":"And","e1":{"opType":"True$"},"e2":{"opType":"False$"}}
     """.trim

     val actual = expression.toJson

     assert(actual.isSuccess)
     assert(actual.get == expected)
  }
  
  "toJson" should "return successful result for complex BooleanExpression" in {
     val expression = Not(Or(And(True, False), Variable("var")))
     val expected = """
      {"opType":"Not","e":{"opType":"Or","e1":{"opType":"And","e1":{"opType":"True$"},"e2":{"opType":"False$"}},"e2":{"opType":"Variable","symbol":"var"}}}
      """.trim

     val actual = expression.toJson

     assert(actual.isSuccess)
     assert(actual.get == expected)
  }
  
  "fromJson" should "return success result for valid JSON" in {
     val json = """
      {"opType":"Not","e":{"opType":"Or","e1":{"opType":"And","e1":{"opType":"True$"},"e2":{"opType":"False$"}},"e2":{"opType":"Variable","symbol":"var"}}}
      """.trim
     val expected = Not(Or(And(True, False), Variable("var")))

     val actual = BooleanExpression.fromJson(json)

     assert(actual.isSuccess)
     assert(actual.get == expected)
  }
  
  "fromJson" should "return failure result for invalid JSON" in {
     val json = """
      {invalidJson} 
     """.trim

     val actual = BooleanExpression.fromJson(json)

     assert(actual.isFailure)
  }
  
  "fromJsonHandleError" should "return empty string for invalid JSON" in {
     val json = """
      {invalidJson} 
     """.trim

     val actual = BooleanExpression.fromJsonHandleError(json)
     val expected = ""

     assert(actual == expected)
  }
  
  "toJsonHandleError" should "return successful result for atomic BooleanExpression" in {
     val expression = And(True, False)
     val expected = """
      {"opType":"And","e1":{"opType":"True$"},"e2":{"opType":"False$"}}
     """.trim

     val actual = expression.toJsonHandleError

     assert(actual == expected)
  }
  
  "fromJsonHandleError" should "return success result for valid JSON" in {
     val json = """
      {"opType":"Not","e":{"opType":"Or","e1":{"opType":"And","e1":{"opType":"True$"},"e2":{"opType":"False$"}},"e2":{"opType":"Variable","symbol":"var"}}}
      """.trim
     val expected = Not(Or(And(True, False), Variable("var")))

     val actual = BooleanExpression.fromJsonHandleError(json)

     assert(actual == expected)
  }

}
