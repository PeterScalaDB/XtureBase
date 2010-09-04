/**
 * Author: Peter Started:29.08.2010
 */
package definition.comm

/** commands that are send by the server
 * 
 */
object ServerCommands extends Enumeration {
	val sendTypes=Value("send Types")
	val getSetupData=Value("set Setup Data")
  val sendQueryResponse=Value("sendQueryResponse")
  val sendOrderedData=Value("send ordered Data")
  val acceptSubscription=Value("Accept subscription")
  val sendSubscriptionNotification=Value("send notification")
  val wantQuit=Value("want quit")

}