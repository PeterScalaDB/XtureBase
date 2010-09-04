/**
 * Author: Peter Started:29.08.2010
 */
package transaction.handling
import server.storage.TransLogHandler

import server.comm._
import server.storage.StorageManager
import server.config._
import definition.typ.AllClasses



/** the main class for server
 * 
 */
object SessionManager {

  def main(args: Array[String]): Unit = { 
  	init()
  }
  
  
  def init() = {
  	AllClasses.fromXML( xml.XML.loadFile(FSPaths.configDir+"types.xml" ))
  	UserList.fromXML(xml.XML.loadFile(FSPaths.configDir+"users.xml" ))
  	StorageManager.init(AllClasses.getClassList)
  	CommonSubscriptionHandler.init(AllClasses.getClassList)
  	println("Max Trans:"+TransLogHandler.transID)
  	
  	Runtime.getRuntime.addShutdownHook(new Thread {
      override def run = { 
      	
      	shutDown()
      }
    })

  	
  	MainServerSocket.start()
  	//MainServerSocket.join()
  	
  }
  
  def shutDown() = {
  	// notify all users to quit connection
  	println("Shutting Down Server")  	
  	ActiveUsers.shutDown(() => {
  	 StorageManager.shutDown()	
  	}  	)
  	Thread.sleep(1000)
  	println("finish")
  }	
  	
  	
  	
  
}