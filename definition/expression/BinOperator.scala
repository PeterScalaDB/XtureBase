/**
 * Author: Peter Started:23.07.2010
 */
package definition.expression

import definition.typ.DataType

/** Abstract trait of all operators for binary operations
 * 
 */
abstract class BinOperator(val opChar:Char,val level:Byte) {
	def getValue(left:Constant,right:Constant):Option[Constant]
	override def toString=opChar.toString
}

object BinOperator
{
	/** finds the operator object with the given char
	 * 
	 * @param op name of the operator to find
	 */
	def getOp(op:Char) =
	{
		if (oList contains op) oList(op)
		else throw new IllegalArgumentException("Operator "+op+" is unknown")
	}
	private val oList=Map (
		('+' -> new BinOperator('+',1){
			def getValue(left:Constant,right:Constant)= {
				left.getType match 
  			{
  				case DataType.IntTyp => Some(new IntConstant (left.toInt +right.toInt))
  				case DataType.DoubleTyp => Some(new DoubleConstant (left.toDouble + right.toDouble ))
  				case DataType.StringTyp => Some(new StringConstant (left.toString + right.toString ))
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for add operation ")
  			} 			
			}
		}),
		('-' -> new BinOperator('-',1){
			def getValue(left:Constant,right:Constant)= {
				left.getType match   			
				{
  				case DataType.IntTyp => Some(new IntConstant (left.toInt -right.toInt))
  				case DataType.DoubleTyp => Some(new DoubleConstant (left.toDouble - right.toDouble ))  				
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for add operation ")
  			} 			
			}
		}),
		('*' -> new BinOperator('*',2){
			def getValue(left:Constant,right:Constant)= {
				left.getType match 
  			{
  				case DataType.IntTyp => Some(new IntConstant (left.toInt *right.toInt))
  				case DataType.DoubleTyp => Some(new DoubleConstant (left.toDouble * right.toDouble ))  				
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for add operation ")
  			} 			
			}
		})
	)
}