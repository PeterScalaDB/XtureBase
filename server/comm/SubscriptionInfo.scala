/**
 * Author: Peter Started:04.09.2010
 */
package server.comm

import server.config._
import definition.data._
import server.comm._

/** Contains Information about one subscription
 * 
 */
abstract class SubscriptionInfo(val user:UserEntry,val id:Int,val parentRef:Reference) 

class SingleSubscription(override val user:UserEntry,override  val id:Int,override val parentRef:Reference) 
extends SubscriptionInfo(user,id,parentRef) {
	override def equals(other: Any) = other match {
		case that: SingleSubscription => that.canEqual(this) && this.user == that.user && this.id==that.id && this.parentRef==that.parentRef
		case _ => false
	}
	override def hashCode = 41 * user.hashCode + 1041*id+4041*parentRef.hashCode+3
	override def toString = "SingleSubs("+id+ " ref: "+parentRef+")"
	def canEqual(that: SingleSubscription) = true  
}
object SingleSubsciption {
	// Boilerplate automatically provided by case class you may want to retain
	def apply(user:UserEntry,id:Int,parentRef:Reference) = new SingleSubscription(user,id,parentRef)
	
	// Your extractor
	def unapply(el: SingleSubscription) = Some(el.user,el.id,el.parentRef)
	
}



case class PropSubscription(override val user:UserEntry,override val id:Int,override val parentRef:Reference,propertyField:Byte) 
extends SubscriptionInfo (user,id,parentRef)

/** a subscription for path views
 * 
 */
case class PathSubscription(override val user:UserEntry,override  val id:Int,var path:IndexedSeq[Reference]) 
extends SingleSubscription(user,id,null) {
	def updatePath(newPath:IndexedSeq[Reference]) = {
		//new PathSubscription(user,id,newPath)
		path=newPath
		this
	}
	override def toString = "PathSubs(id:"+id+ " path: "+path.mkString+")"
}