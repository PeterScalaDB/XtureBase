/**
 * Author: Peter Started:29.08.2010
 */
package transaction.handling
import server.storage.TransLogHandler

import server.comm._
import server.storage._
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
  	val sc=new ServerClassList( xml.XML.loadFile(FSPaths.configDir+"types.xml" ))
  	AllClasses.set(sc)  	
  	UserList.fromXML(xml.XML.loadFile(FSPaths.configDir+"users.xml" ))
  	StorageManager.init(sc.classList)
  	CommonSubscriptionHandler.init(AllClasses.get.getClassList.toMap)
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