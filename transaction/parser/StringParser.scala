/**
 * Author: Peter Started:23.07.2010
 */
package transaction.parser

import scala.util.parsing.combinator.JavaTokenParsers
import definition.expression._

/** parses a String into an expression
 * 
 */
class StringParser extends JavaTokenParsers {
	
	def intNumber: Parser[String] = 
    """-?\d+""".r
  def doubleNumber: Parser[String] = 
    """((\d+)?[\.,]\d*|\d*[\.,]\d+)""".r
    
  /*def fieldReference: Parser[String] =
  	"""[#]([tT]\d+)?([iI]\d+)?[fF]\d+""".r*/
	
  def expr: Parser[Expression] =
        (term ~ "+" ~ expr) ^^ { case lhs~plus~rhs => BinaryOperation( lhs,BinOperator.getOp('+'), rhs) } |
        (term ~ "-" ~ expr) ^^ { case lhs~minus~rhs => BinaryOperation( lhs,BinOperator.getOp('-'), rhs) } |
        term 

  def term: Parser[Expression] =
        (factor ~ "*" ~ term) ^^ { case lhs~times~rhs => BinaryOperation( lhs,BinOperator.getOp('*'), rhs) } |
        (factor ~ "·" ~ term) ^^ { case lhs~times~rhs => BinaryOperation( lhs,BinOperator.getOp('*'), rhs) } |
        (factor ~ "/" ~ term) ^^ { case lhs~div~rhs => BinaryOperation( lhs,BinOperator.getOp('/'), rhs) } |
        stringLiteral ^^ { x => { val s=x.toString; new StringConstant(s.substring(1,s.length-1)) }} |
        factor
        
  def fieldRef:Parser[Expression] =
  	   ("[#][Tt]".r ~> intNumber ~ ("[iI]".r ~>intNumber) ~ ("[fF]".r~> intNumber) ) ^^ {
        	case typ~inst~field => new FieldReference(Some(typ.toInt),Some(inst.toLong),field.toByte) 
        } |
        ("[#][iI]".r ~>intNumber ~ ("[fF]".r~> intNumber) ) ^^ {
        	case inst~field => new FieldReference(None,Some(inst.toLong),field.toByte) 
        } |
        ("[#][fF]".r~> intNumber ) ^^ {
        	case field => new FieldReference(None,None,field.toByte) 
        }

  def factor : Parser[Expression] =  	   
  	   doubleNumber ^^ {y => DoubleConstant(y.replace(',','.').toDouble) } |
       intNumber ^^ {x => IntConstant(x.toInt)} |       
        "(" ~> expr <~ ")" | failure("Unknown Element") |
       fieldRef 
       
	
}

object StringParser extends StringParser
{
	def parse(text : String):Expression = parseAll(expr, text).get
}