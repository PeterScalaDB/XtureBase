/**
 * Author: Peter Started:26.06.2010
 */
package definition.typ

/**
 * Enum of all possible data types for an expression and data field
 */


object DataType extends Enumeration 
{		
	val IntTyp=Value("Integer")	
	val LongTyp=Value("Long")
	val DoubleTyp=Value("Double")
	val CurrencyTyp=Value("Currency")
	val StringTyp=Value("String")
	val BoolTyp=Value("Boolean")
	val DateTyp=Value("Date")
	val ObjectRefTyp=Value("ObjRef")	
	val undefined=Value("UNDEFINED")
	val BinOp=Value("BinOp")
	val FieldRefTyp=Value("FieldRef")
	val FunctionCall=Value("FuncCall")
	val CollFunctionCall=Value("CollFuncCall")
	
	def isCompatible(one:Value,other:Value):Boolean ={		
		if(one.id>other.id) return isCompatible(other,one)
		if(one.id==other.id) return true
		
		one match {
			case IntTyp => other match {
				case LongTyp =>true
				case DoubleTyp => true
				case CurrencyTyp => true
				case _ => false
			}
			case LongTyp => other match {
				case DoubleTyp | CurrencyTyp => true
				case _ => false
			}
			case DoubleTyp => other match {
				case CurrencyTyp => true
				
			}
			case StringTyp => true
			case BoolTyp => other match {
				case IntTyp | LongTyp | BoolTyp => true
				case _ => false 
			}
			case _ => false
		}		
	}
}


