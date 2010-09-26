/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import java.io.{DataInput,DataOutput}
import definition.typ.DataType
/** base type for all expression classes
 * 
 */
trait Expression 
{
	def getType:DataType.Value
	
	def getValue:Constant
	
	def createCopy:Expression
	
	def getChildCount:Int
	
	def getChildNr(ix:Int):Expression
	
	def getTerm:String
	
	def isConstant:Boolean
	
	def write(file:DataOutput):Unit
	
	
	
	
	/** returns all Elements of the given type from this expression
	 * 
	 * @param whatType Type-Id from definition.typ.DataType
	 * @param resultList a List of elements of the given type. The elements of this expression will be added to that list.
	 * @return a new list containing all elements of resultList and the elements of this list fitting to the given type
	 */
	def getElementList[T <: Expression](whatType:DataType.Value,resultList:List[T]):List[T]={
		if(this.getType==whatType) this.asInstanceOf[T]::resultList
		else resultList
	}
	
		
	
	/** replaces certain elements of this term
	 * 
	 * @param checker a function that is given a certain part of this term and it gives a replacement
	 * @return the new term with all replacements
	 */
	def replaceExpression(checker:(Expression) => Expression): Expression = 
		checker(this)
}

object Expression
{
	def read(file:DataInput):Expression =
	{
		DataType(file.readByte()) match {
			case DataType.IntTyp => new IntConstant(file.readInt)
			case DataType.LongTyp => new LongConstant(file.readLong)
			case DataType.DoubleTyp => new DoubleConstant(file.readDouble)
			case DataType.StringTyp => new StringConstant(file.readUTF)			
			case DataType.BinOp => BinaryOperation(file)
			case DataType.FieldRefTyp => FieldReference(file)
			case DataType.FunctionCall => FunctionCall(file)
			case DataType.CollFunctionCall => CollectingFuncCall(file)
			case DataType.BoolTyp => new BoolConstant(file.readBoolean)
			case _ => EMPTY_EX
		}		
	}
	
	def readConstant(file:DataInput):Constant =
	{
		DataType(file.readByte()) match {
			case DataType.IntTyp => new IntConstant(file.readInt)
			case DataType.LongTyp => new LongConstant(file.readLong)
			case DataType.DoubleTyp => new DoubleConstant(file.readDouble)
			case DataType.StringTyp => new StringConstant(file.readUTF)
	    case DataType.BoolTyp => new BoolConstant(file.readBoolean)		
			case _ => EMPTY_EX
		}		
	}
}




