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
	
	/** removes a certain FieldReference from the term and replaces it with it's cache Value
	 * 
	 * @param checker a checker function that checks if a given field ref is the one to remove
	 * has to be called by FieldReference instances of this term
	 * @return a new Term with the FieldReference removed
	 */
	def replaceFieldRefWithValue(checker:(FieldReference)=> Boolean):Expression = this
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
			case DataType.FunctionCall => FunctionCall(file)
			case _ => EMPTY_EX
		}		
	}
}




