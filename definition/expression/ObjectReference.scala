/**
 * Author: Peter Started:28.05.2011
 */
package definition.expression
import definition.data.Reference
import definition.typ.DataType
import java.io.{DataOutput,DataInput}

/**
 * 
 */
case class ObjectReference(typ:Int, instance:Int) extends Constant {
  override def toObjectReference=new Reference(typ,instance)
  
  def toInt:Int=typ  
  def toLong:Long=instance  
  def toDouble:Double=toInt.toDouble  
  def toBoolean:Boolean =toInt>0
  def getNative:Any=this
  def getType=DataType.ObjectRefTyp
  def createCopy=new ObjectReference(typ,instance)
  def getTerm="("+typ+","+instance+")"
  
  override def write(file: DataOutput ) = {
  	file.writeByte(DataType.ObjectRefTyp.id)
  	file.writeInt(typ);file.writeInt(instance)
  }
}

object ObjectReference {
	def apply (file: DataInput) = {
		new ObjectReference(file.readInt,file.readInt)
	}
	def apply (ref:Reference)=new ObjectReference(ref.typ,ref.instance)
}