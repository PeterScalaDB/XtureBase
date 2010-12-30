/**
 * Author: Peter Started:27.12.2010
 */
package server.print

import definition.data.{Reference,InstanceData,OutputDefinition,FormDescription}
import server.comm.UserSocket
import definition.typ.SystemSettings
import server.storage.StorageManager
import definition.typ.AllClasses
import java.io.DataOutput
import definition.comm.GeneratorType

/**
 * 
 */
object PrintGenerator {
	val context=new PrintContext
	val lock=new Object
	val dataEater=new DataEater
	
	var printableType:Int = -1
	var printFormType = -1
	
	private var printableClassesMap=collection.mutable.HashMap[Int,Boolean]()
	
	def generatePages(u:UserSocket,dataParent:InstanceData,oDef:OutputDefinition,pageWidth:Int,pageHeight:Int,form:FormDescription) = 
		lock.synchronized {
		if(printFormType== -1) printFormType=SystemSettings().systemTypes("PrintForm")
		val formRef=new Reference(printFormType,oDef.formInst)
		
		val iteratorList=StorageManager.getInstanceProperties(formRef) match {
			case Some(pData) => pData.propertyFields (0).propertyList .map(a=> PrintIterator(form,StorageManager.getInstanceData(a)))
			case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
		}
		//println("Iterators :"+iteratorList.mkString("\n"))
		dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form)
		//dataEater.initPrinting(pageWidth,pageHeight,form)
		iteratorList.foreach(iterateOneIterator(_,dataParent.ref))
		val pagesData=dataEater.getPagesData
		//println("\nPrintresult:\n"+pagesData.mkString("\n"))
		u.sendGeneratedData(write(dataParent.toString))		
	}
	
	def write(title:String)(out:DataOutput)= {
		  out.writeInt(GeneratorType.printGenerator .id)
			out.writeUTF(title)
			dataEater.write(out)
		}
	
	
	
	def iterateOneIterator(it:PrintIterator,dataRef:Reference):Unit= {
		println("iterate "+it+" Data:"+dataRef)
		if(it.forType==dataRef.typ ){
			val instData=StorageManager.getInstanceData(dataRef)
			context.setCurrInstance(instData)
			
			it.beforeStamps.foreach(stamp => {
				stamp.updateVariables(context)
				dataEater.addStamp(stamp, it.horOrient )
			})
			
			val childData=StorageManager.getInstanceProperties(dataRef) match {
				case Some(pData)=> {
					val startPropField=if(isClassPrintable(dataRef.typ))1 else 0
					var ret:Seq[Reference]=Seq.empty
					// put children of all propfields together
					for(i <-startPropField until pData.propertyFields .size)
						ret=ret ++ pData.propertyFields (i).propertyList
					ret
				}
				case None => Seq.empty
			}
			//println("childData "+childData.mkString("; "))
			if(! childData.isEmpty)	
				for(childIt <-it.childIterators) {
					for(cd <-childData.filter(_.typ == childIt.forType )) {
						iterateOneIterator(childIt,cd)
					}					
				}
			
			it.afterStamps.foreach(stamp => {
				stamp.updateVariables(context)
				dataEater.addStamp(stamp, it.horOrient )
			})
		}
	}
	
	// in printable classes the first property field should be ignored
	// this map caches the info what classes are printable
	def isClassPrintable(classType:Int):Boolean= {
		if(printableClassesMap.contains(classType)) printableClassesMap(classType)
		else {
			val isPrintable=AllClasses.get.getClassByID(classType).inheritsFrom(printableType)
			printableClassesMap(classType)=isPrintable
			isPrintable
		}
	}

}


