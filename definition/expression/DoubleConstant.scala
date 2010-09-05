/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/**
 * 
 */
case class DoubleConstant(n:Double) extends Constant {

  
  def getType(): DataType.Value = { DataType.DoubleTyp }

  def createCopy(): Expression = { new DoubleConstant(n) }

  def getTerm() = { String.valueOf(n) }
  
  override def toString=String.valueOf(n)
  
  /*override def equals(other: Any)= other match{ 
                                                case that:DoubleConstant=> n==that.n
                                                case _ => false}
  
  def canEqual(other: Any)=other.isInstanceOf[DoubleConstant]

  override def hashCode = n.hashCode*/
  
  def toInt =  n.round.toInt
  
  def toLong =  n.round.toLong
  
  def toDouble = n
  
  def toBoolean= n>0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.DoubleTyp.id)
  	//println("DoubleID:" +DataType.DoubleTyp.id)
  	file.writeDouble(n)
  }
  
}


