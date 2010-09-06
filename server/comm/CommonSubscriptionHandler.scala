/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import scala.collection.mutable.ArrayBuffer
import server.config._
import definition.data._
import definition.typ._

/**
 * 
 */
object CommonSubscriptionHandler {
	private var subscriptionList= ArrayBuffer[SubscriptionInfo] ()  
	private val listLock : AnyRef = new Object()
	
	private var classHandlerMap:Map[Int,ClassSubscriptionHandler]=Map()
	
	def init (classList:Map[Int,ObjectClass] ) =   {
  	if(classHandlerMap.isEmpty)
  	{  		
  	  classHandlerMap=classHandlerMap++( for (i <-classList.valuesIterator) yield (i.id -> new ClassSubscriptionHandler(i.id)))
  	}
	}
	
	/** adds a new Subscription to the common List
	 * 
	 */
	def addSubscription(user:UserEntry,parentRef:Reference,propertyField:Byte):Int = 
		listLock.synchronized {
			val newS= ( if(propertyField== -1) 
					new SingleSubscription(user,subscriptionList.size,parentRef)			
				else new PropSubscription(user,subscriptionList.size,parentRef,propertyField) )
			subscriptionList += newS		
			classHandlerMap(parentRef.typ ).addSubscription(newS)		
			subscriptionList.size -1
		}
	
	
	def removeSubscription(id:Int) = 
		listLock.synchronized {	
			val subs=subscriptionList(id)
			if(subs==null) throw new IllegalArgumentException("Removing Subscription "+id+" but it is null")
			classHandlerMap(subs.parentRef.typ).removeSubscription(subs)			
			subscriptionList(id)=null			
		}
	
	
	def getSubscriptionInfo(id:Int):SubscriptionInfo =
		listLock.synchronized {
			subscriptionList(id)
		}
	
	
	def instanceChanged(newState:InstanceData) = {
		//println("subsMan inst changed "+newState.ref)
		// notify subscriptions for this single instance
		classHandlerMap(newState.ref.typ ).singleInstanceChanged(newState)
		// notify subscriptions for this instance as child
		for(owner <-newState.owners )
			classHandlerMap(owner.ownerRef.typ).childInstanceChanged(owner.ownerRef,owner.ownerField,newState)
	}
	
	
	def instanceCreated(owner:OwnerReference,newInstance:InstanceData) = {
		//println("subsMan inst created "+ newInstance.ref)
		if(classHandlerMap.contains(owner.ownerRef.typ))
		classHandlerMap(owner.ownerRef.typ).instanceCreated(owner,newInstance)		
	}
	
	
	def instanceDeleted(owner:OwnerReference,ref:Reference) = {
		//println("subsMan inst deleted "+ ref)
		classHandlerMap(owner.ownerRef.typ).instanceDeleted(owner,ref)
	}
	
	
	def userLogsOff(userID:Int) = listLock.synchronized {		
		println("subsMan user log off "+userID)
		for(subs <- subscriptionList; if (subs!=null))
			if(subs.user.info.id==userID) {				
				classHandlerMap(subs.parentRef.typ).removeSubscription(subs)
				subscriptionList(subs.id)=null 
			}
	}
}