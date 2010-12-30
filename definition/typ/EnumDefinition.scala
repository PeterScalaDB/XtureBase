/**
 * Author: Peter Started:18.12.2010
 */
package definition.typ

import definition.data.InstanceData
import server.storage.StorageManager
import java.io.{DataOutput,DataInput}
import scala.collection.JavaConversions._
/**
 * 
 */
class EnumDefinition (val name:String,val id:Int,val enumValues:collection.Map[String,Int]) {
  override def toString= name+"("+id+")"
  lazy val getElem:Map[Int,(String,Int)]=enumValues.map(a=>(a._2->a)).toMap 
  lazy val javaVect=new java.util.Vector[(String,Int)](enumValues.toSeq)
  
  def write(out:DataOutput)= {
		out.writeUTF(name)
		out.writeInt(id)
		out.writeInt(enumValues.size)
		for(e <-enumValues){
			out.writeUTF(e._1 )
			out.writeInt(e._2)
		}
	}
}

object EnumDefinition {
	
	def apply(data:InstanceData)= {
		val name= data.fieldValue(1).toString
		val id= data.fieldValue(0).toInt
		val enumValues=collection.mutable.LinkedHashMap[String,Int]()
		StorageManager.getInstanceProperties(data.ref) match {
			case Some(propData)=> {
				for(pField<-propData.propertyFields(0).propertyList) {
					val dat=StorageManager.getInstanceData(pField)
					  enumValues(dat.fieldValue(1).toString)= dat.fieldValue(0).toInt
				}
			}
			case None => println("EnumDef "+name+" is empty !");Map.empty
		}
		new EnumDefinition(name,id,enumValues)
	}
	
	def apply(in:DataInput)= {
		new EnumDefinition(in.readUTF,in.readInt,{
			val enumValues=collection.mutable.LinkedHashMap[String,Int]()
			for(i <-0 until in.readInt) enumValues (in.readUTF)=in.readInt
			enumValues
		})
	}
	
}

object NOENUM extends EnumDefinition("Undefined",0,Map.empty){
	override def toString()=""
}