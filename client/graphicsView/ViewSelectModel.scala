/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import client.dialog._

/**
 * 
 */
class ViewSelectModel(controller:GraphViewController) extends SelectSender {
	private val elList=collection.mutable.ArrayBuffer[GraphElem]()
	
	private val listeners=collection.mutable.HashSet[SelectListener]()
	
	def list=elList
	
	def deselect(notify:Boolean)= {
		elList.clear
		controller.stopModus()
		if(notify) notifyListeners
	}
	
	def addSelection(newElems:Seq[GraphElem]) ={
		for(ne<-newElems){
			val ix=list.indexOf(ne)
			if(ix<0) list +=ne // was not in list, add it
			else list.remove(ix)
		}	
		notifyListeners
	}
	
	def registerSelectListener(listener:SelectListener)= {
		listeners+=listener
	}
	
	def elemRemoved(elem:GraphElem) = {
		val ix=list.indexOf(elem)
		if(ix> -1) list.remove(ix)
	}
	
	def elemChanged(oldEl:GraphElem,newEl:GraphElem)={
		val ix=list.indexOf(oldEl)
		if(ix> -1) list(ix)=newEl
	}
	
	
	
	def notifyListeners= listeners.foreach(_.selectionChanged(this,elList))
	
	

}