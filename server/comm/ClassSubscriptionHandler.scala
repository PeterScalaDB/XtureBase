/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import scala.collection.mutable._
import definition.data._

/** handles subscriptions for this class
 * 
 */
class ClassSubscriptionHandler(typID:Int) {
	// subscriptions to single objects
	var singleSubsMap=Map[Reference,List[SingleSubscription]]()
	// subscriptions to parents
	var propSubsMap=Map[Reference,List[PropSubscription]]()
	
	def addSubscription(s:SubscriptionInfo) = {
		println("csm addSubs:"+s)
		s match {
			case a:SingleSubscription => addSingleS(a)
			case b:PropSubscription => addPropS(b)
		}				
	}
	
	def addPathSubscription(p:PathSubscription,ref:Reference) = {
		println("csm addPathSubs:"+p+ " => "+ref)
		if(singleSubsMap.contains(ref )) { // add to existing
			val list=singleSubsMap(ref)
			singleSubsMap.put(ref,p :: list)
		} else // add new
		singleSubsMap.put(ref ,List(p))
	}
	
	def removeSubscription(s:SubscriptionInfo) = {		
		s match {
			case c:PathSubscription => removePathS(c)
			case a:SingleSubscription => removeSingleS(a)
			case b:PropSubscription => removePropS(b)
		}
	}
	
	
	def singleInstanceChanged(newState:InstanceData) = {
		if (singleSubsMap.contains(newState.ref))	{
			val list=singleSubsMap(newState.ref)
			for(subs <-list)
				subs.user.queryHandler.notifyInstanceChanged(subs,newState)
		}
	}
	
	def childInstanceChanged(ownerRef:Reference,propField:Byte,childInst:InstanceData) = {
		if(propSubsMap.contains(ownerRef)) {
			val list=propSubsMap(ownerRef)
			for(subs <-list)
				if(subs.propertyField ==propField)
					subs.user.queryHandler.notifyInstanceChanged(subs,childInst)
		}
	}
	
	/*def burstNotifyChanges(ownerRef:Reference ,propField:Byte) = {
		if(propSubsMap.contains(ownerRef)) {
			val list=propSubsMap(ownerRef)
			for(subs <-list)
				if(subs.propertyField ==propField)
					subs.user.queryHandler.burstNotifyChanges(subs,ownerRef,propField)
		}
	}*/
	
	def instanceCreated(owner:OwnerReference,newInstance:InstanceData) = {
		if(propSubsMap.contains(owner.ownerRef)) {
			val list=propSubsMap(owner.ownerRef)
			for(subs <-list)
				if(subs.propertyField ==owner.ownerField )
					subs.user.queryHandler.notifyInstanceAdded(subs,newInstance)
		}
	}
	
	def instanceDeleted(owner:OwnerReference,ref:Reference) = {
		if(owner==null) { // check single subscriptions
			if(singleSubsMap.contains(ref)) {
				val list=singleSubsMap(ref)
				for(subs <-list)
					subs.user.queryHandler.notifyInstanceDeleted(subs,ref)
				singleSubsMap.remove(ref)	
			}				
			if(propSubsMap.contains(ref)) { // a parent ref of a subscription is deleted
				val list=propSubsMap(ref)
				for(subs <-list)
					subs.user.queryHandler.notifyInstanceDeleted(subs,ref)
				propSubsMap.remove(ref)
			}	
		} // check property subscriptions
		else if( propSubsMap.contains(owner.ownerRef)) {
			val list=propSubsMap(owner.ownerRef)
			for(subs <-list)
				if(subs.propertyField ==owner.ownerField )
					subs.user.queryHandler.notifyInstanceDeleted(subs,ref)
		}
		
	}
	
	// ********************** Internal routines ***********************
	
	private def addSingleS(s:SingleSubscription) = {
		if(singleSubsMap.contains(s.parentRef )) { // add to existing
			val list=singleSubsMap(s.parentRef)
			singleSubsMap.put(s.parentRef,s :: list)
		} else // add new
		singleSubsMap.put(s.parentRef ,List(s))
	}
	
	private def addPropS(s:PropSubscription) = {
		if(propSubsMap.contains(s.parentRef )) { // add to existing
			val list=propSubsMap(s.parentRef)
			propSubsMap.put(s.parentRef,s :: list)
		} else // add new
		propSubsMap.put(s.parentRef ,List(s))
	}
	
	private def removeSingleS(s:SingleSubscription) = {
		println("csm remove singleSubs:"+s)
		if(singleSubsMap.contains(s.parentRef)) {
			val list=singleSubsMap(s.parentRef)
			if(list.contains(s))
				singleSubsMap.put(s.parentRef,list.filterNot(_ ==  s))
		}
		else println("Entry for "+s.parentRef+" not found")
	}
	
	private def removePropS(s:PropSubscription) = {
		println("csm remove propSubs:"+s)
		if(propSubsMap.contains(s.parentRef)) {
		val list=propSubsMap(s.parentRef)
		if(list.contains(s))
			propSubsMap.put(s.parentRef,list.filterNot(_ == s))
		} else println("Entry for "+s.parentRef+" not found")
	}	
	
	def removePathS(s:PathSubscription) = {
		println("csm remove pathSubs:"+s)
		for((k,list) <-singleSubsMap.iterator)
			if(list.contains(s))
				 singleSubsMap.put(k,list.filterNot (_ == s))
	}
}

object ClassSubscriptionHandler {
	val ALLFIELDS= (-2).toByte
}