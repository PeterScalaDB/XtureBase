/**
 * Author: Peter Started:05.09.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}
/**
 * 
 */
case class LongConstant(val n:Long) extends Constant {

  def getType(): DataType.Value = { DataType.LongTyp }

  def createCopy(): Expression = { new LongConstant(n) }

  def getTerm() = { String.valueOf(n) }
  
  override def toString=String.valueOf(n)
  
  /*override def equals(other: Any)= other match{ 
                                                case that:IntConstant=> n==that.n
                                                case _ => false}
  
  def canEqual(other: Any)=other.isInstanceOf[IntConstant]

  override def hashCode = n.hashCode*/
  
  def toInt =  n.toInt
  
  def toDouble = n.toDouble
  
  def toLong = n
  
  def toBoolean= n>0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.LongTyp.id)
  	file.writeLong(n)
  }
  def getNative=n
}