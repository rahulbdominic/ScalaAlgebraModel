package scalaalgebramodel

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

  def toJson(expression: BooleanExpression) = {
    Serialization.write(expression)
  }

  def fromJson(json: String) = {
    Serialization.read[BooleanExpression](json)
  }
}

sealed trait BooleanExpression 


case object True extends BooleanExpression

case object False extends BooleanExpression

case class Variable(symbol: String) extends BooleanExpression

case class Not(e: BooleanExpression) extends BooleanExpression

case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

