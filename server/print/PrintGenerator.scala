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
import transaction.handling.TransactionManager
import server.storage.ActionNameMap
import definition.data.OwnerReference
import definition.expression._
import java.util.Date

/**
 * 
 */
object PrintGenerator {
	val context=new PrintContext
	val lock=new Object
	val dataEater=new DataEater
	
	var printableType:Int = -1
	var printFormType = -1
	var archiveType = -1
	var iteratorType = -1
	var sectionIteratorType= -1
	var parentIteratorType= -1
	
	private var printableClassesMap=collection.mutable.HashMap[Int,Boolean]()
	
	
	def generatePages(u:UserSocket,dataParent:InstanceData,oDef:OutputDefinition,pageWidth:Int,pageHeight:Int,form:FormDescription) = 
		lock.synchronized {			
		if(printFormType== -1) {
			printFormType=SystemSettings().systemTypes("PrintForm")		
			archiveType=SystemSettings().systemTypes("PrintArchive")
			iteratorType=SystemSettings().systemTypes("PrintIterator")
			sectionIteratorType=SystemSettings().systemTypes("SectionIterator")
			parentIteratorType=SystemSettings().systemTypes("ParentIterator")
		}
		context.initPrintSession()
		val formRef=new Reference(printFormType,oDef.formInst)
		context.setPrintDate(new DateConstant(new Date))
		val iteratorList=StorageManager.getInstanceProperties(formRef) match {
			case Some(pData) => pData.propertyFields (0).propertyList .map(a=> PrintIterator(form,StorageManager.getInstanceData(a)))
			case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
		}
		
		dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form,context)		
		iteratorList.foreach(iterateOneIterator(_,dataParent.ref))
		val pagesData=dataEater.getPagesData		
		u.sendGeneratedData(write(dataParent.toString,oDef.odInst))		
	}
	
	
	def storePages(u:UserSocket,dataParent:InstanceData,oref:Reference,oDef:OutputDefinition,pageWidth:Int,pageHeight:Int,form:FormDescription) = 
		lock.synchronized {
		context.initPrintSession()	
		if(printFormType== -1) printFormType=SystemSettings().systemTypes("PrintForm")
		val formRef=new Reference(printFormType,oDef.formInst)
		context.setPrintDate(new DateConstant(new Date))
		val iteratorList=StorageManager.getInstanceProperties(formRef) match {
			case Some(pData) => pData.propertyFields (0).propertyList .map(a=> PrintIterator(form,StorageManager.getInstanceData(a)))
			case None => throw new IllegalArgumentException("cant find Iterators in Form "+formRef+" "+oDef)
		}
		
		dataEater.initPrinting(if(oDef.portrait)pageWidth else pageHeight,if(oDef.portrait)pageHeight else pageWidth,form,context)		
		iteratorList.foreach(iterateOneIterator(_,dataParent.ref))
		val pagesData=dataEater.getPagesData	
		
		TransactionManager.doTransaction(u.userEntry.info.id, ActionNameMap.getActionID("Ausdruck archivieren"),oref, false, archiveType,{
			var archiveInst=TransactionManager.tryCreateInstance(archiveType, Array(new OwnerReference(1,oref)), true, -1, true, true)
			archiveInst=archiveInst.setField(0,new StringConstant((<fl>{form.fonts.toXML}</fl>).toString))
			archiveInst=archiveInst.setField(2,new IntConstant(if(oDef.portrait)pageWidth else pageHeight))
			archiveInst=archiveInst.setField(3,new IntConstant(if(oDef.portrait)pageHeight else pageWidth))
			archiveInst=archiveInst.setField(4,new BoolConstant(form.isLandscape))
			archiveInst=archiveInst.setField(5,new IntConstant(form.left))
			archiveInst=archiveInst.setField(6,new IntConstant(form.top))
			archiveInst=archiveInst.setField(7,new IntConstant(form.right))
			archiveInst=archiveInst.setField(8,new IntConstant(form.bottom))
			archiveInst=archiveInst.setField(9,BlobConstant.fillData(dataEater.write ))
			archiveInst=archiveInst.setField(10,new DateConstant(new Date))
			TransactionManager.tryWriteInstanceData(archiveInst)
		})
		//u.sendGeneratedData(write(dataParent.toString))
		
	}
	
	def write(title:String,odefInst:Int)(out:DataOutput)= {
		  out.writeInt(GeneratorType.printGenerator .id)
			out.writeUTF(title)
			out.writeInt(odefInst)
			dataEater.write(out)
		}
	
	
	
	def iterateOneIterator(it:PrintIterator,dataRef:Reference):Unit= {		
		//println("iterate "+it+" Data:"+dataRef)
		//val theClass=AllClasses.get.getClassByID(dataRef.typ )
		
		//if(theClass.inheritsFrom(  it.forType) ){
		val instData=StorageManager.getInstanceData(dataRef)
		it.iterate(instData,dataEater,context)
		/*	
			context.setCurrInstance(instData)
			it match {
				case s:SectionIterator=> {					
					dataEater.currentHeader =s.headerStamps .firstOption
					dataEater.currentFooter =s.footerStamps .firstOption
					dataEater.initSection
				}
				case _=>
			}
			
			it.beforeStamps.foreach(stamp => {
				stamp.updateVariables(context)
				dataEater.addStamp(stamp, it.horOrient )
			})
			
			StorageManager.getInstanceProperties(dataRef) match {
				case Some(pData )=> {
					for(childIt <-it.childIterators) {
						val pfield=childIt.propField 
						for(cData <-pData.propertyFields(pfield).propertyList) {
							val childClass=AllClasses.get.getClassByID(cData.typ)
							if(childClass.inheritsFrom(childIt.forType ))
								iterateOneIterator(childIt,cData)
						}
							
					}
				}
				case _ =>
			}						
			
			if(!it.afterStamps.isEmpty) {
				context.setCurrInstance(instData)
				it.afterStamps.foreach(stamp => {
					stamp.updateVariables(context)
					dataEater.addStamp(stamp, it.horOrient )
				})
			}
			
			it match {
				case s:SectionIterator=> {
					dataEater.addPage
				}
				case _=>
			}
		//}*/
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


