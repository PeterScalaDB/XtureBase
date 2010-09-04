/**
 * Author: Peter Started:29.08.2010
 */
package definition.comm

/** commands that are send by clients
 * 
 */
object ClientCommands extends Enumeration {
  val getTypes=Value("Get Types")
  val getSetupData=Value("Get Setup Data")
  val storeSetupData=Value("Store Setup Data")
  val queryInstance=Value("Query Instance")
  val startSubscription=Value("Start Subscription")
  val changeSubscription=Value("Change Subscription")
  val stopSubscription=Value("Stop Subscription")
  val createInstance=Value("create Instance")
  val writeField=Value("write Field")
  val deleteInstance=Value("delete Instance")
  val logOut=Value("logout")
  
}