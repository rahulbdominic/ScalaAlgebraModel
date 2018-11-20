package scalaalgebramodel

import scala.util.{Try, Success, Failure}

object BooleanExpressionUtil {
  import org.json4s._
  import org.json4s.jackson.Serialization
  
  implicit val formats = new DefaultFormats {
    override val typeHintFieldName = "opType"
    override val typeHints = ShortTypeHints(List(
      True.getClass, 
      False.getClass, 
      classOf[Variable],
      classOf[Not],
      classOf[Or],
      classOf[And]
    ))
  }

  // Returning a Result is a better API than having a side 
  // effect and printing the error here. However, another functions
  // toJsonHandleError and fromJsonHandleError is also provided below
  def toJson(expression: BooleanExpression) = {
    Try {
      Serialization.write(expression)
    } 
  }

  def fromJson(json: String) = {
    Try {
      Serialization.read[BooleanExpression](json)
    }
  }

  def toJsonHandleError(expression: BooleanExpression) = {
    val result = Try {
      Serialization.write(expression)
    } 

    result match {
      case Success(v) => v
      case Failure(e) => { 
        println(s"Serialization error: ${e}")
        ""
      }
    }
  }

  def fromJsonHandleError(json: String) = {
    val result = Try {
      Serialization.read[BooleanExpression](json)
    }

    result match {
      case Success(v) => v
      case Failure(e) => {
        println(s"Serialization error: ${e}")
        ""
      }
    }
  }
}

sealed trait BooleanExpression 


case object True extends BooleanExpression

case object False extends BooleanExpression

case class Variable(symbol: String) extends BooleanExpression

case class Not(e: BooleanExpression) extends BooleanExpression

case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

