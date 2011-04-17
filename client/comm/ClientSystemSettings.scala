/**
 * Author: Peter Started:18.12.2010
 */
package client.comm

import java.io.{DataInput}
import definition.typ.{EnumDefinition,NOENUM,SystemSettings}
import definition.data.Reference
/**
 * 
 */
class ClientSystemSettings(in:DataInput) extends SystemSettings {
  val _systemTypes:collection.Map[String,Int]= 
  	(for(i <-0 until in.readInt) yield (in.readUTF->in.readInt)).toMap
  	
	val enums:Map[String,EnumDefinition]= 
		(for(i <-0 until in.readInt) yield (in.readUTF->EnumDefinition(in))).toMap
		
		
		
	val enumByID=(enums.map(a=> (a._2.id,a._2)))
	
	val clientSettingsMap:Map[String,String]=(for(i <-0 until in.readInt) 
		yield (in.readUTF ->in.readUTF)).toMap
	
	println("system types:"+_systemTypes.mkString(","))
	println("Enums:"+enums.mkString("| "))
	println("ClientSettings:"+clientSettingsMap.mkString("| "))
	
	def getCustomSettings(folderName:String):IndexedSeq[Reference]= IndexedSeq.empty
	
	def getClientSetting(settingName:String):String=
		if(clientSettingsMap.contains(settingName)) clientSettingsMap(settingName)
		else ""
			
	def systemTypes(key:String):Int = if(_systemTypes.contains(key)) _systemTypes(key)
	 else throw new IllegalArgumentException("cant find SystemType "+key)		
}


