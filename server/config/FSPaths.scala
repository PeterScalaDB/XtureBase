/**
 * Author: Peter Started:27.06.2010
 */
package server.config

import java.util.prefs._
import java.io.File

/** Contains the directories in the file system
 * 
 */
object FSPaths 
{
	val systemNode=Preferences.userNodeForPackage(this.getClass);
	
	val dirNode =systemNode.node("directories");	
	
  val configDir=checkSeparator( dirNode.get("ConfigDir","c:\\"))
  
  val dataDir=checkSeparator( dirNode.get("DataDir","c:\\temp\\"))
  
  
  private def checkSeparator(path:String) =  	
  	if(path.last==File.pathSeparatorChar) path else path+File.separator
  
}