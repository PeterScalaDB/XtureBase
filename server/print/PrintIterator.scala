/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import java.io.{DataInput,DataOutput}
import definition.data.{Reference,InstanceData,FormDescription}
import server.storage.StorageManager
import definition.typ.AllClasses

/**
 * 
 */
class PrintIterator(val form:FormDescription,val propField:Byte,val forType:Int,val horOrient:Boolean,val sortFeld:Int,val beforeStamps:Seq[Stamp],
	val afterStamps:Seq[Stamp],	val childIterators:Seq[PrintIterator]) {
	
	def fullToString= "Iterator for type "+forType+" horOrient:"+horOrient+" sortFeld:"+sortFeld+"\n beforeStamps:"+
	beforeStamps.mkString("\n")+"\n afterStamps:"+afterStamps.mkString("\n")+"\n childIterators:"+childIterators.mkString("\n")
	
	override def toString= "Iterator for type "+forType+" numBefore:"+beforeStamps.size+" numAfter:"+afterStamps.size+
	" ChildItTypes:"+childIterators.map(_.forType).mkString(",")
	
	def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit = {
		context.setCurrInstance(instData)
		beforeStamps.foreach(stamp => {
			stamp.updateVariables(context)
			dataEater.addStamp(stamp, horOrient )
		})

		StorageManager.getInstanceProperties(instData.ref) match {
			case Some(pData )=> {
				for(iter <-childIterators) 
					iter match {
						case p:ParentIterator =>{
							p.iterate(instData, dataEater, context)
						}
						case childIt => {
							val pfield=childIt.propField 
							for(cData <-pData.propertyFields(pfield).propertyList) {
								val childClass=AllClasses.get.getClassByID(cData.typ)							
								if(childClass.inheritsFrom(childIt.forType )){
									childIt.iterate(StorageManager.getInstanceData(cData),dataEater,context)
								}								
							}	
						}						
				}
			}
			case _ =>
		}						

		if(!afterStamps.isEmpty) {
			context.setCurrInstance(instData)
			afterStamps.foreach(stamp => {
				stamp.updateVariables(context)
				dataEater.addStamp(stamp, horOrient )
			})
		}
	}
}

class SectionIterator(nform:FormDescription,npropField:Byte,nforType:Int,nhorOrient:Boolean,nsortFeld:Int,nbeforeStamps:Seq[Stamp],nafterStamps:Seq[Stamp],	
	val headerStamps:Seq[Stamp],val footerStamps:Seq[Stamp],	nchildIterators:Seq[PrintIterator]) extends 
	PrintIterator(nform,npropField,nforType,nhorOrient,nsortFeld,nbeforeStamps,nafterStamps,nchildIterators) {
	//println(this)
	override def fullToString= "SectionIterator for type "+forType+" horOrient:"+horOrient+" sortFeld:"+sortFeld+"\n beforeStamps:"+
	beforeStamps.mkString("\n")+"\n afterStamps:"+afterStamps.mkString("\n")+"\n headerStamps:"+headerStamps.mkString("\n")+
	"\n footerStamps:"+footerStamps.mkString("\n")+"\n childIterators:"+childIterators.mkString("\n")
	
	override def toString= "SectionIterator for type "+forType+" numBefore:"+beforeStamps.size+" numAfter:"+afterStamps.size+" numHeader:"+headerStamps.size+
	" numFooter:"+footerStamps.size+	" ChildItTypes:"+childIterators.map(_.forType).mkString(",")
	
	override def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit = {
		context.setCurrInstance(instData)								
		dataEater.currentHeader =headerStamps .firstOption
		dataEater.currentFooter =footerStamps .firstOption
		dataEater.initSection
		super.iterate(instData,dataEater,context)
		dataEater.addPage				
	}
}

class ParentIterator(nform:FormDescription,npropField:Byte,nforType:Int,nhorOrient:Boolean,nsortFeld:Int,nbeforeStamps:Seq[Stamp],nafterStamps:Seq[Stamp],
	nchildIterators:Seq[PrintIterator],val parentType:Int) extends 
	PrintIterator(nform,npropField,nforType,nhorOrient,nsortFeld,nbeforeStamps,nafterStamps,nchildIterators) {
	//println(this)
	override def toString= "parentIterator for type "+forType+" numBefore:"+beforeStamps.size+" numAfter:"+afterStamps.size+
	" ChildItTypes:"+childIterators.map(_.forType).mkString(",")+" parentType:"+parentType
	
	override def iterate(instData:InstanceData,dataEater:DataEater,context:PrintContext):Unit = {
		println("parentIterator iter:"+instData)
		StorageManager.getNextParentOfType(instData.ref, parentType) match {
			case Some(parent)=>{
				println("parent:"+parent)
				super.iterate(StorageManager.getInstanceData(parent), dataEater, context)
			}
			case None => throw new IllegalArgumentException("ParentIterator cant find Parent of type "+parentType+" in Ref:"+instData.ref)
			
		}
		
	}
	
}


object PrintIterator {
	def apply(form:FormDescription,data:InstanceData)={		
		if(data.ref.typ == PrintGenerator.iteratorType )
		new PrintIterator(form,data.fieldValue(4).toInt.toByte,data.fieldValue(0).toInt,data.fieldValue(1).toBoolean,data.fieldValue(2).toInt,
			loadStamps(form,data.ref,0.toByte),loadStamps(form,data.ref,1.toByte),loadChildren(form,data.ref))
		else if(data.ref.typ == PrintGenerator.sectionIteratorType )
		new SectionIterator(form,data.fieldValue(4).toInt.toByte,data.fieldValue(0).toInt,data.fieldValue(1).toBoolean,data.fieldValue(2).toInt,
			loadStamps(form,data.ref,0.toByte),loadStamps(form,data.ref,1.toByte),loadStamps(form,data.ref,3.toByte),
			loadStamps(form,data.ref,4.toByte),loadChildren(form,data.ref))	
		else if(data.ref.typ == PrintGenerator.parentIteratorType )
		new ParentIterator(form,data.fieldValue(4).toInt.toByte,data.fieldValue(0).toInt,data.fieldValue(1).toBoolean,data.fieldValue(2).toInt,
			loadStamps(form,data.ref,0.toByte),loadStamps(form,data.ref,1.toByte),loadChildren(form,data.ref),data.fieldValue(5).toInt)	
		
		else throw new IllegalArgumentException("loading PrintIterators type "+data.ref+" is not allowed")
	}
		
		
		
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

