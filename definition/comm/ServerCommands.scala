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
  val sendCommandResponse=Value("sendCommandResponse")
  //val sendOrderedData=Value("send ordered Data")
  val acceptSubscription=Value("Accept subscription")
  //val acceptPathSubscription=Value("Accept path subscription")
  val sendSubscriptionNotification=Value("send notification")
  //val sendPathSubsNotification=Value("send path notification")
  val wantQuit=Value("want quit")

}