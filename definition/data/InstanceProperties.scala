/**
 * Author: Peter Started:28.07.2010
 */
package definition.data

import java.io._
import scala.collection.immutable.Vector


/** Stores the Property Information of an instance
 * 
 */
class InstanceProperties(override val ref:Reference, val propertyFields:Array [PropertyFieldData])
      extends Referencable {
	
	override def write(file:DataOutput) =	{
		file.writeByte(propertyFields.length)
		for(p <- propertyFields)
			p.write(file)
	}	
	
	def addChildInstance (field:Byte,newInst:Reference):InstanceProperties = {
		if(field>=propertyFields.length) throw new 
		    IllegalArgumentException("Can't add children: No Property Field #"+field+ " in instance "+ref)
		val newFields=propertyFields.clone
		newFields(field)=newFields(field).addPropertyInstance(newInst)
		new InstanceProperties(ref,newFields)
	}
	
	def removeChildInstance (field:Byte,remInst:Reference):InstanceProperties = {
		val newFields=propertyFields.clone
		newFields(field)=newFields(field).removePropertyInstance(remInst)
		new InstanceProperties(ref,newFields)   
	}
	
	   
}


object InstanceProperties 
{
	var pman:APropertyManager=null
	
	
	 def read(ninstRef:Reference,file:DataInput  ) = {
		val count:Int=file.readByte
		val propArray=new Array[PropertyFieldData](count.toInt)
		for(i <-0 until count)
			 propArray(0)=PropertyFieldData(file)
	  new InstanceProperties(ninstRef,propArray)				
	}
		
}