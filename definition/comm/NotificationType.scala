/**
 * Author: Peter Started:03.09.2010
 */
package definition.comm

/** Notification Types for Subscriptions
 * 
 */
object NotificationType extends Enumeration {
	val sendData=Value("Send data")
	val FieldChanged=Value("Field Changed")
	val childAdded=Value("Child Added")
	val instanceRemoved=Value("Child Removed")
	//val parentRemoved=Value("Parent Removed")

}