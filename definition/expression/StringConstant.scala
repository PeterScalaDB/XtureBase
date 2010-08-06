/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/**
 *  String Constant type
 */
class StringConstant(val n:String) extends Constant {

  def toInt(): Int = {  n.toInt }

  def toDouble(): Double = { n.toDouble }

  def toBoolean(): Boolean = { n.toBoolean }

  def getType(): DataType.Value = { DataType.StringTyp  }

  def createCopy(): Expression = { new StringConstant(n) }

  def getTerm(): String = { '"'+n+'"' }

  override def toString=n
  
  override def equals(other: Any)= other match{ 
                                                case that:StringConstant=> n==that.n
                                                case _ => false}
  
  def canEqual(other: Any)=other.isInstanceOf[StringConstant]
  
  override def hashCode = n.hashCode
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.StringTyp.id)
  	//println("StringID:" +DataType.StringTyp.id)
  	//Thread.dumpStack()
  	file.writeUTF(n)
  }
}