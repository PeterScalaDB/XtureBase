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
	val VectorTyp=Value("Vector")
	val EnumTyp=Value("Enum")
	val ParentRefTyp=Value("ParentRef")
	val BlobTyp=Value("Blob")
	
	def isCompatible(one:Value,other:Value):Boolean ={
		if(one==undefined || other==undefined) return true
		if(one.id>other.id) return isCompatible(other,one)
		if(one.id==other.id) return true
		
		one match {
			case IntTyp => other match {
				case LongTyp =>true
				case DoubleTyp => true
				case CurrencyTyp => true
				case ParentRefTyp => true
				case FieldRefTyp => true
				case _ => false
			}
			case LongTyp => other match {
				case DoubleTyp | CurrencyTyp => true
				case ParentRefTyp => true
				case FieldRefTyp => true
				case _ => false
			}
			case DoubleTyp => other match {
				case CurrencyTyp => true
				case ParentRefTyp => true
				case FieldRefTyp => true
				case _ => false
			}
			case StringTyp => true
			case BoolTyp => other match {
				case IntTyp | LongTyp | BoolTyp => true
				case ParentRefTyp => true
				case FieldRefTyp => true
				case _ => false 
			}
			case _ => false
		}		
	}
	def getValueClass=Value.getClass
	
	lazy val wrapMap:Map[DataType.Value,DTWrap]=values.map(a=>(a -> DTWrap(a.toString,a))).toMap
	lazy val wrappedValues=values.map(a=> DTWrap(a.toString,a)).toSeq
}

case class DTWrap(name:String,typ:DataType.Value) {
	override def toString=name
}


