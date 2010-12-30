/**
 * Author: Peter Started:19.12.2010
 */
package server.print
import server.config.ServerSystemSettings
import definition.data.{Reference,FormDescription,InstanceData}
import definition.typ.SystemSettings
import server.storage.StorageManager

/**
 * 
 */
object PrintFormsHandler {
	val folderType=SystemSettings().systemTypes("Folder")
	val formType=SystemSettings().systemTypes("PrintForm")
	
	val typesList:Map[Int,Seq[FormDescription]]=getForms 
	
	//println("typeslist :"+typesList.mkString(","))
	
		
	def getForms:Map[Int,Seq[FormDescription]]= {
	  val subFolders=SystemSettings().getCustomSettings("PrintForms")	
	  //println("subFolders:"+subFolders)
	  (for(sf<-subFolders;if(sf.typ ==folderType);val dat=StorageManager.getInstanceData(sf))
	  	yield (dat.fieldValue(0).toInt->loadFormsForType(sf))).toMap
	}
	
	def loadFormsForType(typeFolder:Reference):Seq[FormDescription]= {
		StorageManager.getInstanceProperties(typeFolder) match {
			case Some(data)=> for(pField<-data.propertyFields(1).propertyList;if(pField.typ==formType))
							yield {
								val pchildList:Seq[InstanceData]=StorageManager.getInstanceProperties(pField) match {
									case Some(data)=>for(c <-data.propertyFields (2).propertyList) yield StorageManager.getInstanceData(c)
									case None => Seq.empty
								}
								val fchildList:Seq[InstanceData]=StorageManager.getInstanceProperties(pField) match {
									case Some(data)=>for(c <-data.propertyFields (3).propertyList) yield StorageManager.getInstanceData(c)
									case None => Seq.empty
								}
								FormDescription(StorageManager.getInstanceData(pField),pchildList,fchildList)
							}
			case None => Seq.empty
		}
	}
	
	def getForm(dataType:Int,formInst:Int) = typesList(dataType).find(_.inst ==formInst) match {
		case Some(form) => form
		case None => throw new IllegalArgumentException("Cant find Form inst "+formInst +" for Type "+dataType)
	}
}

/*class FormsForType(typeID:Int,forms:Seq[FormDescription]){
	
	def getForm(formInst:Int):Option[FormDescription] = forms.find(_.inst ==formInst)
}*/
