/**
 * Author: Peter Started:24.04.2011
 */
package definition.expression


import definition.typ.DataType
import java.io.{DataOutput,DataInput}
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * 
 */
case class BlobConstant(val data:Array[Byte]) extends Constant {
	
  def getType(): DataType.Value = { DataType.BlobTyp  }

  def createCopy(): Expression = {
  	val nArray=new Array[Byte](data.size)
  	java.lang.System.arraycopy(data, 0, nArray, 0, data.size)
  	new BlobConstant(nArray)
  }

  def getTerm() = toString
  
  override def toString="[Blob size="+data.size+"]"
  
  /*override def equals(other: Any)= other match{ 
                                                case that:IntConstant=> n==that.n
                                                case _ => false}
  
  def canEqual(other: Any)=other.isInstanceOf[IntConstant]

  override def hashCode = n.hashCode*/
  
  def toInt =  0
  
  def toDouble = 0d 
  
  def toLong = 0l
  
  def toBoolean= false
  
  def write(file:DataOutput)= {  	
  	file.writeByte(DataType.BlobTyp.id)
  	file.writeInt(data.size)
  	file.write(data)
  }
  def getNative=data
  
  override def isNumberConstant=true
  
  def fillData(func:(DataOutput)=>Unit):BlobConstant= {
  	val byteStream=new ByteArrayOutputStream() 
  	val outStream=new DataOutputStream(byteStream)
  	func(outStream)  	
  	new BlobConstant(byteStream.toByteArray)
  }
}

object BlobConstant {
	def fillData(func:(DataOutput)=>Unit):BlobConstant= {
  	val byteStream=new ByteArrayOutputStream() 
  	val outStream=new DataOutputStream(byteStream)
  	func(outStream)  	
  	new BlobConstant(byteStream.toByteArray)
  }
	def apply(file:DataInput)= {
		val length=file.readInt
		val newArray=new Array[Byte](length)
		file.readFully(newArray)
		new BlobConstant(newArray)
	}
}