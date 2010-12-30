/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/**
 *  String Constant type
 */
case class StringConstant(val n:String) extends Constant {

  def toInt(): Int = try{  n.toInt } catch { case e => 0}
  
  def toLong(): Long = try { n.toLong } catch { case e => 0L}

  def toDouble(): Double = try{ n.toDouble } catch { case e => 0}

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
  	//System.out.println("StringID:" +DataType.StringTyp.id)
  	//Thread.dumpStack()
  	file.writeUTF(n)
  }
  def getNative=n
}