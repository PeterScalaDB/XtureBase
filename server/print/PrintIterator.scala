/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import java.io.{DataInput,DataOutput}

import definition.data.{Reference,InstanceData,FormDescription}
import server.storage.StorageManager

/**
 * 
 */
case class PrintIterator(form:FormDescription,forType:Int,horOrient:Boolean,sortFeld:Int,beforeStamps:Seq[Stamp],afterStamps:Seq[Stamp],
	childIterators:Seq[PrintIterator]) {
	
	def fullToString= "Iterator for type "+forType+" horOrient:"+horOrient+" sortFeld:"+sortFeld+"\n beforeStamps:"+
	beforeStamps.mkString("\n")+"\n afterStamps:"+afterStamps.mkString("\n")+"\n childIterators:"+childIterators.mkString("\n")
	
	override def toString= "Iterator for type "+forType+" numBefore:"+beforeStamps.size+" numAfter:"+afterStamps.size+
	" ChildItTypes:"+childIterators.map(_.forType).mkString(",")
}

object PrintIterator {
	def apply(form:FormDescription,data:InstanceData)= new PrintIterator(form,data.fieldValue(0).toInt,data.fieldValue(1).toBoolean,data.fieldValue(2).toInt,
		loadStamps(form,data.ref,0.toByte),loadStamps(form,data.ref,1.toByte),loadChildren(form,data.ref))
		
		
	def loadStamps(form:FormDescription,parent:Reference,propField:Byte):Seq[Stamp] = {
		StorageManager.getInstanceProperties(parent) match {
			case Some (pData)=> pData.propertyFields (propField).propertyList .map(a=> new Stamp(form,StorageManager.getInstanceData(a)))
			case None => Seq.empty
		}
	}	

	def loadChildren(form:FormDescription,parent:Reference):Seq[PrintIterator] = {
		StorageManager.getInstanceProperties(parent) match {
			case Some (pData)=> pData.propertyFields (2).propertyList .map(a=> PrintIterator(form,StorageManager.getInstanceData(a)))
			case None => Seq.empty
		}
	}	
}