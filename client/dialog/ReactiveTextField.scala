/**
 * Author: Peter Started:29.11.2010
 */
package client.dialog

import scala.swing.TextField
import scala.swing.event.EditDone

/**
 * 
 */
class ReactiveTextField(func:(String)=>Unit) extends TextField {
  listenTo(this)
  reactions+={
  	case e:EditDone =>  func(text)  		
  }  
}