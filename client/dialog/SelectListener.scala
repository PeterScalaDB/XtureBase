/**
 * Author: Peter Started:09.10.2010
 */
package client.dialog

import definition.data._


/** a  component that sends out selection messages 
 * 
 */

trait SelectSender {
	def deselect(notify:Boolean)
	def registerSelectListener(listener:SelectListener)
}


/** a component that receives select messages
 * 
 */
trait SelectListener {
	def selectionChanged(sender:SelectSender,instList:Seq[Referencable])
}