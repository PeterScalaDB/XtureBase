/**
 * Author: Peter Started:10.11.2010
 */
package client.dialog
import definition.data.Referencable

/** collects all select events and sends them to consumers
 * 
 */
object SelectEventDispatcher extends SelectListener with SelectSender{
	val listenerList= collection.mutable.HashSet[SelectListener]()
	var lastSender:SelectSender = null
	
	def deselect(notify:Boolean) = {
		if(lastSender!=null)lastSender.deselect(notify)
	}
	def registerSelectListener(listener:SelectListener) = {
		listenerList += listener
	}
	
	
	def selectionChanged(sender:SelectSender,instList:Seq[Referencable]) = {
	  for( li <-listenerList) li.selectionChanged(sender,instList)
	  lastSender=sender
	}

}