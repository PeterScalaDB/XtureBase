/**
 * Author: Peter Started:10.11.2010
 */
package client.dialog
import definition.data.Referencable
import definition.data.OwnerReference
import definition.typ.SelectGroup

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
	
	def removeSelectListener(listener:SelectListener) = {
		listenerList -=listener
	}
	
	
	def selectionChanged [T <: Referencable](sender:SelectSender,groups:Seq[SelectGroup[T]]) = {
	  for( li <-listenerList) li.selectionChanged(sender,groups)
	  lastSender=sender
	}

}