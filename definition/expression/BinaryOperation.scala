/**
 * Author: Peter Started:23.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/** Binar Operation expression
 * 
 */
case class BinaryOperation(left:Expression,operator:BinOperator,right:Expression) extends Expression {

  def getType(): DataType.Value = { null }
  
  private var cachedValue:Option[Constant]=None

  def getValue(): Constant = 
  { 
  	val value1=left.getValue
  	val value2=right.getValue
  	cachedValue = operator.getValue(value1,value2)  	
  	cachedValue match { case Some(value) => value
  											case None => null
  											}
  }
  
  

  def createCopy(): Expression = { new BinaryOperation(left.createCopy,operator,right.createCopy) }

  def getChildCount(): Int = { 2 }

  def getChildNr(ix: Int): Expression = { ix match {case 0 => left;case 1=>right }}
  
  private def checkParentheses(arg:Expression):String =
  {
     arg match {
    	 case bi:BinaryOperation if (bi.operator.level<operator.level) => "("+arg.getTerm+")"
    	 case _ => arg.getTerm
     }
  }

  def getTerm(): String = {  checkParentheses(left) + " "+operator+" "+checkParentheses(right) }

  def isConstant(): Boolean = { false }
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.BinOp.id)
  	file.writeChar(operator.opChar )
  	left.write(file)
  	right.write(file)
  }
  
  // looks for fieldreferences in both operands
  override def getFieldReferences (resultList:List[FieldReference]) = {
  	left.getFieldReferences(right.getFieldReferences(resultList))
  }
  
  override def replaceFieldRefWithValue(checker:(FieldReference)=> Boolean):Expression = {
  	new BinaryOperation(left.replaceFieldRefWithValue(checker),operator,
  		right.replaceFieldRefWithValue(checker))
  }
  
}

object BinaryOperation
{
	def apply (file: DataInput):Expression =
	{
		val op=file.readChar()
	  new BinaryOperation	(Expression.read(file),BinOperator.getOp(op),Expression.read(file))
	}
}