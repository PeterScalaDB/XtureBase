/**
 * Author: Peter Started:28.07.2010
 */
package definition.data

import java.io._
import scala.collection.immutable.Vector


/** Stores the Property Information of an instance
 * propertyFields: Array of the propertyFieldsData of an instance
 */
class InstanceProperties(override val ref:Reference, val propertyFields:Array [PropertyFieldData])
      extends Referencable {
	
	//TODO: convert to indexseq
	
	override def write(file:DataOutput) =	{
				
		file.writeByte(propertyFields.length)
		for(p <- propertyFields)
			p.write(file)			
	}	
	
	def dataLength:Int = {
		var result=1
		for(p<-propertyFields) result += p.dataLength
		result
	}
	
	def addChildInstance (field:Byte,newInst:Reference,atPos:Int):InstanceProperties = {
		if(field>=propertyFields.length) throw new 
		    IllegalArgumentException("Can't add children: No Property Field #"+field+ " in instance "+ref)
		val newFields=propertyFields.clone
		newFields(field)=newFields(field).addPropertyInstance(newInst,atPos)
		new InstanceProperties(ref,newFields)
	}
	
	def removeChildInstance (field:Byte,remInst:Reference):InstanceProperties = {
		val newFields=propertyFields.clone
		newFields(field)=newFields(field).removePropertyInstance(remInst)
		new InstanceProperties(ref,newFields)   
	}
	
	def moveChildInstanceToPos (field:Byte,inst:Reference,pos:Int):InstanceProperties = {
		val newFields=propertyFields.clone
		newFields(field)=newFields(field).moveInstanceToPos(inst,pos)
		new InstanceProperties(ref,newFields)   
	}
	
	def hasChildren:Boolean = {
		for (p <-propertyFields) if(! p.propertyList.isEmpty ) return true
		return false
	}
	
	override def toString = "InstProp" +ref.toString+" "+propertyFields.mkString(",")
}


object InstanceProperties 
{
	
	
	 def read(ninstRef:Reference,file:DataInput  ) = {
		val count:Int=file.readByte
		val propArray=new Array[PropertyFieldData](count.toInt)
		for(i <-0 until count)
			 propArray(i)=PropertyFieldData(file)
	  new InstanceProperties(ninstRef,propArray)				
	}
		
}