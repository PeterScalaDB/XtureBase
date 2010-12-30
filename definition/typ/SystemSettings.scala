/**
 * Author: Peter Started:18.12.2010
 */
package definition.typ
import definition.data.Reference


/**
 * 
 */
trait SystemSettings {   
	def systemTypes(key:String):Int	
	def enums:collection.Map[String,EnumDefinition]
	def enumByID:collection.Map[Int,EnumDefinition]
	
	def genIDMap=(enums.map(a=> (a._2.id,a._2)))
	
	def getCustomSettings(folderName:String):IndexedSeq[Reference]
	
	def getClientSetting(settingName:String):String
}

object SystemSettings {
	var settings:SystemSettings= _
	def apply()=settings
}