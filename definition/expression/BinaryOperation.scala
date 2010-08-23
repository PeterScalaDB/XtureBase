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

  def getType(): DataType.Value = { DataType.BinOp  }
  
  

  def getValue(): Constant = 
  { 
  	val value1=left.getValue
  	val value2=right.getValue
  	operator.getValue(value1,value2)  match { 
  		case Some(value) => value
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
  
    
  override def getElementList[T <: Expression](whatType:DataType.Value,resultList:List[T]):List[T]={
  	left.getElementList(whatType,right.getElementList(whatType,super.getElementList(whatType,resultList)))		
	}
  
  
  
  override def replaceExpression(checker:(Expression) => Expression): Expression = 
  {
    val newMe=checker(this)
    if (newMe==this) // was not changed
    {
    	val newLeft=left.replaceExpression(checker)
    	val newRight=right.replaceExpression(checker)
    	if(newLeft!=left || newRight!=right) // operators were changed
    		new BinaryOperation(newLeft,operator,newRight)
    	else this
    }
    else newMe // return replacement of me
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