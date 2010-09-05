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
	
	def addSubscription(s:SubscriptionInfo) = s match {
		case a:SingleSubscription => addSingleS(a)
		case b:PropSubscription => addPropS(b)
	}
	
	def removeSubscription(s:SubscriptionInfo) = s match {
		case a:SingleSubscription => removeSingleS(a)
		case b:PropSubscription => removePropS(b)
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
		val list=singleSubsMap(s.parentRef)
		singleSubsMap.put(s.parentRef,list - s)
	}
	
	private def removePropS(s:PropSubscription) = {
		val list=propSubsMap(s.parentRef)
		propSubsMap.put(s.parentRef,list - s)
	}
	
	
	
	
	
}