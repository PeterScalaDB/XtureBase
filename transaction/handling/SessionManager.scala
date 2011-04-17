/**
 * Author: Peter Started:29.08.2010
 */
package transaction.handling
import server.storage.TransLogHandler
import server.comm._
import server.storage._
import server.config._
import definition.typ.{AllClasses,SystemSettings}
import definition.expression.FunctionManager
import runtime.function.CommonFuncMan


/** the main class for server
 * 
 */
object SessionManager {
  var scl:ServerClassList=null
  def main(args: Array[String]): Unit = { 
  	init()
  }
  
  var isSetup=false
  
  val setupListeners=collection.mutable.ArrayBuffer[()=>Unit]()
  
  
  
  def registerSetupListener(func:()=>Unit) = {
  	//System.out.println("register setup "+func+" isSetup:"+isSetup)
  	if(isSetup){
  		//System.out.println("call direct")
  		func() // if the classes List is ready, call the func
  	}
  	else setupListeners +=func // else wait for getting set up
  }
  
  def init() = {
  	System.out.println("Sessionmanager init")  	
  	scl=new ServerClassList( xml.XML.loadFile(FSPaths.configDir+"types.xml" ))
  	AllClasses.set(scl)  	
  	UserList.fromXML(xml.XML.loadFile(FSPaths.configDir+"users.xml" ))
  	StorageManager.init(scl.classList)
  	ActionNameMap.read
  	//System.out.println(ActionNameMap)
  	//System.out.println("transDetail:")
  	//System.out.println(TransDetailLogHandler.readFully.mkString("\n"))
  	//System.out.println("ready")
  	//System.out.println("TimeLog \n"+TimeLogger.readFully.map(a => " TR:"+a._1+"- "+new java.util.Date(a._2*60000L)).mkString("\n"))
  	CommonSubscriptionHandler.init(AllClasses.get.getClassList.toMap)  
  	SystemSettings.settings=new ServerSystemSettings(FSPaths.settingsObjectRef)
  	//println("call setup"+setupListeners.mkString)
  	for(li <-setupListeners)
  		li() // call listeners
  	isSetup=true	
  	System.out.println("Max Trans:"+TransLogHandler.transID)  	
  	
  	Runtime.getRuntime.addShutdownHook(new Thread {
      override def run = { 
      	
      	shutDown()
      }
    })

  	
  	MainServerSocket.start()
  	//MainServerSocket.join()
  	//converter()
  }
  
  def shutDown() = {
  	// notify all users to quit connection
  	System.out.println("Shutting Down Server")  	
  	ActiveUsers.shutDown(() => {
  	 StorageManager.shutDown()	
  	}  	)
  	Thread.sleep(500)
  	System.out.println("finish")
  }	
  	
  // converts all Referencing links
  /*def converter() {
  	println("Converting referencing Links : ")
  	AllClasses.get.classList.valuesIterator.foreach ( (aClass)=> {
  		StorageManager.getHandler(aClass.id).foreachInstance((ref)=> {
  			StorageManager.getReferencingLinks(ref) match {
  				case Some(data)=> {
  					print(data.ref.sToString+" ")
  					StorageManager.writeReferencingLinks(data)
  				}
  				case None => 
  			}
  		})
  	})  	
  }*/
  	
  
}