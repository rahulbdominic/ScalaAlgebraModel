package scalaalgebramodel

import scala.util.{Try, Success, Failure}
import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer


sealed trait BooleanExpression {
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
  def toJson = {
    Try {
      Serialization.write(this)
    } 
  }
   
  def toJsonHandleError = {
    val result = Try {
      Serialization.write(this)
    } 

    result match {
      case Success(v) => v
      case Failure(e) => { 
        println(s"Serialization error: ${e}")
        ""
      }
    }
  }

  // TODO: Add tests
  def getDisjunctiveNormalForm: BooleanExpression = {
    val (minTerms, variables) = getMinTerms
    val minTermExps = for (minTerm <- minTerms) yield 
      BooleanExpression.minTermToExp(minTerm, variables)

    BooleanExpression.orAll(minTermExps)
  }
  
  // TODO: Add tests
  def getMinTerms = {
    val variables = BooleanExpression.getVariables(this)

    val result = ListBuffer[Int]()
    for (i <- 0 until math.pow(2, variables.size).toInt) {
      val binValue = BooleanExpression.byteToBoolList(i, variables.size)
      val map = variables.zip(binValue).toMap

      if(evalute(map)) {
        result += i
      }
    }

    (result.toList, variables.toList)
  }
  
  // TODO: Add tests
  def evalute(values: Map[String, Boolean]) = {
    def parseTree(that: BooleanExpression): BooleanExpression = that match {
      case Or(e1, e2) => Or(parseTree(e1),  parseTree(e2))
      case And(e1, e2) => And(parseTree(e1), parseTree(e2))
      case Not(e) => Not(parseTree(e))
      case Variable(symb) => if (values(symb)) True else False
      case _ => that 
    }

    def evaluateTree(that: BooleanExpression): Boolean = that match {
      case Or(e1, e2) => evaluateTree(e1) || evaluateTree(e2) 
      case And(e1, e2) => evaluateTree(e1) &&  evaluateTree(e2)
      case Not(e) => !evaluateTree(e)
      case True => true
      case False => false
      case _ => throw new Exception("Invalid BooleanExpression to evaluate") 
    }

    val substitutedTree = parseTree(this)
    evaluateTree(substitutedTree)
  }
} 

case object True extends BooleanExpression

case object False extends BooleanExpression

case class Variable(symbol: String) extends BooleanExpression {
  override def toString = symbol.toUpperCase
}

case class Not(e: BooleanExpression) extends BooleanExpression {
  override def toString = s"(!${e.toString})"
}

case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression {
  override def toString = s"(${e1.toString} OR ${e2.toString})"
}

case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression {
  override def toString = s"(${e1.toString} AND ${e2.toString})"
}

object BooleanExpression {
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

  private def getVariables(exp: BooleanExpression): Set[String] = {
    exp match {
      case Or(e1, e2) => getVariables(e1) ++ getVariables(e2)
      case And(e1, e2) => getVariables(e1) ++ getVariables(e2)
      case Not(e) => getVariables(e)
      case Variable(symb) => Set(symb)
      case _ => Set() 
    }
  }

  private def byteToBoolList(value: BigInt, size: Int): List[Boolean] = {

    def byteToBoolListRec(that: BigInt): List[Boolean] = {
      if(that == 0) {
        Nil
      }
      else {
        List(that.testBit(0)) ::: byteToBoolListRec(that >> 1)
      }
    }

    var listBoolean = List[Boolean]()

    if(value >= 0) {
      listBoolean = byteToBoolListRec(value)
    } else {
      // Do not support numbers less than 0
      listBoolean = List()
    }

    padBoolList(listBoolean, size)
  }

  private def padBoolList(bitList: List[Boolean], size: Int): List[Boolean] = {
    if (bitList.length == size) {
      bitList
    } else if (bitList.length > size) {
      bitList.drop(bitList.length - size)
    } else {
      bitList ::: List.fill(size - bitList.length)(false)
    }
  }

  def fromJson(json: String) = {
    Try {
      Serialization.read[BooleanExpression](json)
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

  // TODO: Add tests
  // Returns False if the expression list is empty
  def orAll(exps: List[BooleanExpression]): BooleanExpression = {
    def buildExp(pairs: List[BooleanExpression]): Option[BooleanExpression] = {
      if (pairs.isEmpty) {
        return None 
      }
      val exp :: rest = pairs

      buildExp(rest) match {
        case Some(restExp) => Some(Or(exp, restExp))
        case None => Some(exp)
      }
    }

    buildExp(exps) match {
        case Some(exp) => exp 
        case None => False
    }
  }

  // TODO: Add tests
  def minTermToExp(minterm: Int, variables: List[String]) = {
    val binValue = byteToBoolList(minterm, variables.size)
    val pairs = variables.zip(binValue).toList

    def buildExp(pairs: List[Tuple2[String, Boolean]]): Option[BooleanExpression] = {
      if (pairs.isEmpty) {
        return None 
      }
      val pair :: rest = pairs
      val (symb, term) = pair
      
      val exp = if (term) Variable(symb) else Not(Variable(symb))

      buildExp(rest) match {
        case Some(restExp) => Some(And(exp, restExp))
        case None => Some(exp)
      }
    }
    
    buildExp(pairs) match {
        case Some(exp) => exp 
        case None => False
    }
  } 
}

