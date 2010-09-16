/**
 * Author: Peter Started:28.07.2010
 */
package definition.data

import java.io._
import scala.collection.immutable.IndexedSeq
/**
 * 
 */
class PropertyFieldData (val isSingle:Boolean,val propertyList: IndexedSeq[Reference]) {


	def addPropertyInstance(ref:Reference) = {
		if(isSingle && propertyList.length==1) 
			throw new IllegalArgumentException("Cant add more than 1 Property to a single property data")					
		new PropertyFieldData(isSingle, propertyList :+ ref)			
	}

	def removePropertyInstance(ref:Reference) = {
    val newList=if(propertyList.length==1 && propertyList.head==ref) IndexedSeq.empty
    else propertyList.filter(x => x!=ref)
		new PropertyFieldData(isSingle,newList)
	}

	/*
	def clearAllProperty() = {
		for(ref <- propertyList) 
			master.pman.removeProperty(master.instRef,ref)
		new PropertyData(master,isSingle,Nil)}	
	 */
	def write(file:DataOutput) =   {
		file.writeBoolean(isSingle)
		file.writeShort(propertyList.size)
		for(p <-propertyList) p.write(file)
	}
	
	def dataLength:Int = {
		3 +propertyList.size*12
	}
		
		
	override def toString() = {
		"["+propertyList.mkString(", ")+"]"
	}



}

object PropertyFieldData
{
	def apply(file:DataInput) = {
		val single=file.readBoolean
		val count=file.readShort
		var plist:IndexedSeq[Reference]=IndexedSeq.empty
		for (i <-0 until count)
			plist= plist :+ Reference(file)
			new PropertyFieldData(single,plist)
	}

}