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

case class SingleSubscription(override val user:UserEntry,override  val id:Int,override val parentRef:Reference) 
extends SubscriptionInfo(user,id,parentRef)

case class PropSubscription(override val user:UserEntry,override val id:Int,override val parentRef:Reference,propertyField:Byte) 
extends SubscriptionInfo (user,id,parentRef)