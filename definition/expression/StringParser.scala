/**
 * Author: Peter Started:23.07.2010
 */
package definition.expression

import scala.util.parsing.combinator._
import definition.expression._
import definition.typ.DataType


/** parses a String into an expression
 * 
 */
class StringParser extends JavaTokenParsers {
	
	def intNumber: Parser[String] = 
    """-?\d+""".r
  def doubleNumber: Parser[String] = 
    """-?((\d+)?[\.,]\d*|-?\d*[\.,]\d+)""".r
    
  def numberParam: Parser[String] = """\d+""".r
  
  def currencySymbols: Parser[String]= """[\u00A3\u20AC]+""".r
    
  /*def fieldReference: Parser[String] =
  	"""[#]([tT]\d+)?([iI]\d+)?[fF]\d+""".r*/
  
  def comp: Parser[Expression] =
  	    (expr ~ "=" ~expr) ^^ {case lhs~equ~rhs => BinaryOperation(lhs,BinOperator.getOp('='),rhs)} |
  	    (expr ~ "<" ~expr) ^^ {case lhs~equ~rhs => BinaryOperation(lhs,BinOperator.getOp('<'),rhs)} |
  	    (expr ~ ">" ~expr) ^^ {case lhs~equ~rhs => BinaryOperation(lhs,BinOperator.getOp('>'),rhs)} |
		    expr
	
	
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
        
  def factor:Parser[Expression] = 
  	    (elem ~ "^" ~ factor) ^^ { case lhs~pow ~rhs => BinaryOperation(lhs,BinOperator.getOp('^'),rhs) } |
  	    elem | failure("Unknown Term-Element")
        
  def fieldRef:Parser[Expression] =
  	   ("[#][Tt]".r ~> intNumber ~ ("[iI]".r ~>intNumber) ~ ("[fF]".r~> intNumber) ) ^^ {
        	case typ~inst~field => new FieldReference(Some(typ.toInt),Some(inst.toInt),field.toByte) 
        } |
        ("[#][iI]".r ~>intNumber ~ ("[fF]".r~> intNumber) ) ^^ {
        	case inst~field => new FieldReference(None,Some(inst.toInt),field.toByte) 
        } |
        ("[#][fF]".r~> intNumber ) ^^ {
        	case field => new FieldReference(None,None,field.toByte) 
        } |failure("wrong Fieldref")
  
  def parentFieldRef:Parser[Expression]= ("[#][Pp]".r ~> intNumber ~("[Ff]".r ~>intNumber)) ^^ {
        	case ownerIx~fieldNr => new ParentFieldRef(ownerIx.toByte,fieldNr.toByte)
        }
        
  def elem : Parser[Expression] =  	   
  	   doubleNumber ^^ {y => DoubleConstant(y.replace(',','.').toDouble) } |
       intNumber ^^ {x => IntConstant(x.toInt)}      |
        "(" ~> comp <~ ")" | currValue| fieldRef | function | collFunction | parentFieldRef | vector | failure("Unknown Factor-Element")
       
	def paramList:Parser[List[Expression]] =
		 ((comp ~ rep(";" ~> comp)) ^^ {  case ex~ list => ex::list.map(_.asInstanceOf[Expression]) }) |
		 (comp ^^ {case ex => List(ex)})
		 
	def currValue: Parser[Expression] = 
		(doubleNumber ~ currencySymbols) ^^ {case a ~ c=> CurrencyConstant(Math.round(a.replace(',','.').toDouble*100L))}	 
		
	def function: Parser[Expression] = 
		(ident ~ "(" ~ paramList ~ ")") ^^ {case name ~ b ~ list ~c => FunctionCall(None,name,list)} 
       
  def collFunction: Parser[Expression] = (("#" ~> ident <~ "(") ~ (numberParam <~ ";") ~ (numberParam <~ ";") ~ (numberParam <~ ")")) ^^ {
  																					case name ~ propField ~ childType ~ childField => 
  																					CollectingFuncCall(name,propField.toByte,childType.toInt,childField.toByte) 
                                         }
  def vector: Parser[Expression] = 
  	( ("""[Vv][\[]""".r~> doubleNumber <~";") ~ (doubleNumber <~";") ~ (doubleNumber <~"]")  ) ^^ {
  																					case x ~ y ~ z  => VectorConstant(x.toDouble,y.toDouble,z.toDouble) 		
  																			 }	
}

object StringParser extends StringParser
{
	def parse(text : String,expectedType:DataType.Value=DataType.undefined):Expression = {
		if(text.length==0) return Expression.generateNullConstant(expectedType) 
		val result=parseAll(comp, text)
		result match {
            case Success(x, _) => return x
            case NoSuccess(err, next) => {
            		throw new IllegalArgumentException("Failure when parsing "+                
                    "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
                    err + "\n" + next.pos.longString)
            }
        }
		
		
	}
}