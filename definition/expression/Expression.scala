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
	
	
	/** returns all FieldReferences from this expression
	 * 
	 */
	def getFieldReferences(resultList:List[FieldReference]):List[FieldReference]=resultList
}

object Expression
{
	def read(file:DataInput):Expression =
	{
		DataType(file.readByte()) match {
			case DataType.IntTyp => new IntConstant(file.readInt)
			case DataType.DoubleTyp => new DoubleConstant(file.readDouble)
			case DataType.StringTyp => new StringConstant(file.readUTF)			
			case DataType.BinOp => BinaryOperation(file)
			case DataType.FieldRefTyp => FieldReference(file)
			case _ => EMPTY_EX
		}		
	}
}




