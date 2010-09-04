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
  		 list.contains(userID)
  	 }
   
   def addUser(newEntry:UserEntry) =  	 {
  	   list.put(newEntry.info.id,newEntry)
  	   println("Listsize: "+list.size)
  	 }
   
   def getUserSocket(userID:Int) = list.get(userID).thread
   
   def getUserQueryHandler(userID:Int) = list.get(userID).queryHandler
   
   def removeUser(userID:Int) = {
  	 list.remove(userID)
  	 println("List size: "+list.size+" " +wantQuit)
  	 checkFinish()
   }
   
   def shutDown(nf:()=>Unit) = {
  	 finalFunc=nf
  	 wantQuit=true
  	 checkFinish()
  	 println("Shutting Down UserList size:" +list.size)
  	 for (u <-list.values.iterator())
  		 u.thread.tellToQuit()
   }
   
   def checkFinish() = if(wantQuit && list.isEmpty() && (finalFunc!=null)) finalFunc()
}