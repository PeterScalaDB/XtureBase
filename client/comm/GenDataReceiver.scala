/**
 * Author: Peter Started:29.12.2010
 */
package client.comm

import java.io.DataInput

/** abstract interface for Receivers of generated data
 * 
 */
trait GenDataReceiver {
   def receiveData(in:DataInput)
}