/**
 * Author: Peter Started:27.06.2010
 */
package server.config

import java.util.prefs._
import java.io.File
import definition.data.Reference

/** Contains the directories in the file system
 * 
 */
object FSPaths 
{
	val systemNode=Preferences.userNodeForPackage(this.getClass);
	
	val dirNode =systemNode.node("directories");	
	
	val setupNode=systemNode.node("setup")
	
  val configDir=checkSeparator( dirNode.get("ConfigDir","config"))
  
  val dataDir=checkSeparator( dirNode.get("DataDir","data"))
  
  val serverPort=setupNode.get("ServerPort","9000").toInt
  
  val settingsObjectRef=Reference(setupNode.get("SettingsObject","30,1"))
  
  
  private def checkSeparator(path:String) =  	
  	if(path.last==File.pathSeparatorChar) path else path+File.separator
  
}