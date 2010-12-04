/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/**
 * 
 */
case class IntConstant(val n:Int) extends Constant {

  def getType(): DataType.Value = { DataType.IntTyp }

  def createCopy(): Expression = { new IntConstant(n) }

  def getTerm() = { String.valueOf(n) }
  
  override def toString=String.valueOf(n)
  
  /*override def equals(other: Any)= other match{ 
                                                case that:IntConstant=> n==that.n
                                                case _ => false}
  
  def canEqual(other: Any)=other.isInstanceOf[IntConstant]

  override def hashCode = n.hashCode*/
  
  def toInt =  n
  
  def toDouble = n.toDouble
  
  def toLong = n.toLong
  
  def toBoolean= n>0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.IntTyp.id)
  	file.writeInt(n)
  }
  def getNative=n
  
  override def isNumberConstant=true
}