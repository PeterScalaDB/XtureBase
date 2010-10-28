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
	
	def init (classList:Map[Int,AbstractObjectClass] ) =   {
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
	
	/** changes the target of a subscription
	 * 
	 */
	def changeSubscription(user:UserEntry,subsID:Int,parentRef:Reference,propertyField:Byte) =  {
	  	removeSubscription(subsID)
	  	listLock.synchronized {
	  		val newS= ( if(propertyField== -1) new SingleSubscription(user,subsID,parentRef)			
										else new PropSubscription(user,subsID,parentRef,propertyField) )
				subscriptionList(subsID) = newS		
				classHandlerMap(parentRef.typ ).addSubscription(newS)			
	  	}
	  }
	
	
	/** adds a suscription for a path element
	 * 
	 */
	def addPathSubscription(user:UserEntry,pathList:IndexedSeq[Reference]): Int = 
		listLock.synchronized {
			val subsID=subscriptionList.size
			val newS= new PathSubscription(user,subsID,pathList)
			subscriptionList+=newS
			// add single Subscriptions to all path elements
			for(el <-pathList)
				classHandlerMap(el.typ).addPathSubscription(newS,el)			
			subsID
		}
	
	/** a path subscription wants to add another child to the path
	 * 
	 */
	def openChild(subsID:Int,newRef:Reference):IndexedSeq[Reference] = 
	  listLock.synchronized {
		 subscriptionList(subsID) match {
			 case subs:PathSubscription => {
				 val newList=subs.path :+ newRef
				 val newSubs=subs.updatePath(newList)
				 subscriptionList(subsID)= newSubs
				 classHandlerMap(newRef.typ).addPathSubscription(newSubs,newRef)
				 newList
			 }
			 case _ => {
				 println("Subscription "+subsID+" is no path subscription when open Child ref"+newRef)
				 null
			 }
		 }		 
	}
	
	/** a path element wants to reduce the path until to a certain element
	 * @param newPos the number of the element in the path that should be the last element
	 */
	def jumpUp(subsID:Int,newPos:Int) = listLock.synchronized {
		subscriptionList(subsID) match {
			 case subs:PathSubscription => {
				 if(subs.path.size<=newPos) println("Wrong jump Pos "+newPos+" when jumping up "+subs.path .mkString("/"))
				 else {
					 for (i <-(newPos+1) until subs.path.size) { // remove unneeded entries
						 val pRef=subs.path(i)
						 classHandlerMap(pRef.typ).removePathS(subs)
					 }
					 subscriptionList(subsID)= subs.updatePath(subs.path.take(newPos+1) ) 
				 }				 
			 }
			 case _ => println("Subscription "+subsID+" is no path subscription when jump Up "+subsID)
		 }
	}
	
	
	def changePath(subsID:Int,newPath:IndexedSeq[Reference]) = listLock.synchronized {
		subscriptionList(subsID) match {
			 case subs:PathSubscription => {
				 // remove old subscription entries
				 for(o <- subs.path)
					 classHandlerMap(o.typ).removePathS(subs)
				 // add new entries
				val newSubs=subs.updatePath(newPath)
				for (n <- newPath)
					classHandlerMap(n.typ).addPathSubscription(newSubs,n) 
				 subscriptionList(subsID)= newSubs				 
			 }
			 case _ => println("Subscription "+subsID+" is no path subscription when changing Path to"+newPath.mkString("/"))
		 }
	}
	
		
	
	def removeSubscription(id:Int) = 
		listLock.synchronized {		
			doRemove(id)						
			subscriptionList(id)=null			
		}
	
	
	def pauseSubscription(id:Int) = 
		listLock.synchronized {	
			doRemove(id)						
		} 
	
	private def doRemove(id:Int) = {
	  	val subs=subscriptionList(id)
			if(subs==null) throw new IllegalArgumentException("Removing Subscription "+id+" but it is null")			
			subs match {
				case p:PathSubscription => 
				for (el <-p.path)
					classHandlerMap(el.typ ).removeSubscription(p)
				case a => classHandlerMap(a.parentRef.typ).removeSubscription(a)					
			}
	}
	
	
	def getSubscriptionInfo(id:Int):SubscriptionInfo =
		listLock.synchronized {
			subscriptionList(id)
		}
	
	// notifies all subscriptionManagers that a notification will start
	def startNotify= {
		
	}
	
	// makes the subscriptionManagers to send their data
	def submitNotifications = {
		
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
		classHandlerMap(ref.typ).instanceDeleted(null,ref)//single
		classHandlerMap(owner.ownerRef.typ).instanceDeleted(owner,ref) // prop
	}
	
	
	def userLogsOff(userID:Int) = listLock.synchronized {		
		println("subsMan user log off "+userID)
		for(subs <- subscriptionList; if (subs!=null))			
			if(subs.user.info.id==userID) {				
				removeSubscription(subs.id)				 
			}
	}
}