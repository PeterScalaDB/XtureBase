/**
 * Author: Peter Started:29.08.2010
 */
package server.comm

import server.config._
import java.util.concurrent._
import scala.collection.JavaConversions._



/** a list of all users that are logged in to the database
 * 
 */
object ActiveUsers {
   private val list =new ConcurrentHashMap[Int,UserEntry]()
   var wantQuit=false
   var finalFunc: ()=>Unit=null
   
   def isOnline(userID:Int):Boolean =	 {  	 	 
  		 list.containsKey(userID)
  	 }
   
   def addUser(newEntry:UserEntry) =  	 {
  	   list.put(newEntry.info.id,newEntry)
  	   System.out.println("addUser "+newEntry+" Listsize: "+list.size)
  	 }
   
   def getUserSocket(userID:Int) = list.get(userID).thread
   
   def getUserQueryHandler(userID:Int) = list.get(userID).queryHandler
   
   def removeUser(userID:Int) = {
  	 val entry=list.get(userID)  	 
  	 list.remove(userID)
  	 CommonSubscriptionHandler.userLogsOff(userID)
  	 System.out.println("Remove user "+userID+"List size: "+list.size+" " +wantQuit)
  	 checkFinish()
   }
   
   def shutDown(nf:()=>Unit) = {
  	 finalFunc=nf
  	 wantQuit=true
  	 checkFinish()
  	 System.out.println("Shutting Down UserList size:" +list.size)
  	 for (u <-list.values.iterator())
  		 u.thread.tellToQuit()
   }
   
   def checkFinish() = if(wantQuit && list.isEmpty() && (finalFunc!=null)) finalFunc()
   
   def lockUsersForUndo(undoUser:UserEntry) = {
  	 for( u<-list.values.iterator();if(u.info.id !=undoUser.info.id))
  		 u.thread.sendUndoLockInfo(undoUser.info .name) 
   }
   
   def releaseUsersForUndo(undoUser:UserEntry) = {
  	 for( u<-list.values.iterator();if(u.info.id !=undoUser.info.id))
  		 u.thread.releaseUndoLock() 
   }
   
}