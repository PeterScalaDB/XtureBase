/**
 * Author: Peter Started:18.12.2010
 */
package server.config
import definition.data.{Reference,InstanceProperties,PropertyFieldData}
import transaction.handling.SessionManager
import server.storage.StorageManager
import definition.typ.{EnumDefinition,NOENUM,SystemSettings}
import java.io.DataOutput
/**
 * 
 */
class ServerSystemSettings(settingsRef:Reference)extends SystemSettings  {
	val _systemTypes=collection.mutable.Map[String,Int]()
	
	var enums=collection.mutable.LinkedHashMap[String,EnumDefinition](("Undefined"->NOENUM))
	var enumByID=collection.Map[Int,EnumDefinition]()
	var topProperties:InstanceProperties= try { 
		StorageManager.getInstanceProperties(settingsRef) match {
			case Some(propData)=> {

				for(pField<-propData.propertyFields(1).propertyList) {
					val dat=StorageManager.getInstanceData(pField)
					_systemTypes(dat.fieldValue(0).toString )= dat.fieldValue(1).toInt
				}
				//println("Types:("+systemTypes.mkString(";")+")")
				for(pField<-propData.propertyFields(0).propertyList;
				val enum=EnumDefinition(StorageManager.getInstanceData(pField))) 
					enums+= (enum.name ->enum)
					enumByID=genIDMap
					//println("Enums:"+enums.mkString("\n"))
				propData
			}
			case None => throw new IllegalArgumentException("No Systemsettings found in "+settingsRef)
		}	
	} catch {
		case e=> System.err.println(e);null
	}
	
	
	lazy val customFolderList= if(topProperties==null)IndexedSeq.empty else for(pField<-topProperties.propertyFields(2).propertyList) 
		yield StorageManager.getInstanceData(pField)
	
	
	def systemTypes(key:String):Int = if(_systemTypes.contains(key)) _systemTypes(key)
	 else throw new IllegalArgumentException("cant find SystemType "+key)
	
	def getCustomSettings(folderName:String):IndexedSeq[Reference]= {
		//println("CustomFoldersList:"+customFolderList)
		for(folder <-customFolderList;if(folder.fieldValue(0).toString.equalsIgnoreCase(folderName)))
			StorageManager.getInstanceProperties(folder.ref) match {
			case Some(data) => return data.propertyFields(1).propertyList
			case None => return IndexedSeq.empty
		}
		println("Folder not found :"+folderName)
		IndexedSeq.empty
	}
	
	lazy val clientSettingsMap:Map[String,String]=(for(child <-getCustomSettings("ClientSettings");data=StorageManager.getInstanceData(child)) 
		yield (data.fieldValue(0).toString ->data.fieldValue(1).toString)).toMap
	
	def getClientSetting(settingName:String):String=
		if(clientSettingsMap.contains(settingName)) clientSettingsMap(settingName)
		else ""
		
	
	def write(out:DataOutput)= {
		out.writeInt(_systemTypes.size)
		for(t<-_systemTypes) {
			out.writeUTF(t._1)
			out.writeInt(t._2)
		}
		out.writeInt(enums.size)
		for(e<-enums) {
			out.writeUTF(e._1)
			e._2.write(out)
		}
		out.writeInt(clientSettingsMap.size)
		for(c<-clientSettingsMap){
			out.writeUTF(c._1)
			out.writeUTF(c._2)
		}
	}
}

