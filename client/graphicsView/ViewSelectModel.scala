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
	
	/** adds the elements to the selection
	 * 
	 * @param newElems the elements to dd
	 * @param toggle should the elements be removed when they are already in the selection ?
	 */
	def addSelection(newElems:Seq[GraphElem],toggle:Boolean) ={
		if(toggle) {
			for(ne<-newElems) {
				val ix=elList.indexOf(ne)
				if(ix<0) elList +=ne
				else elList.remove(ix)
			}				
		}
		else for(ne<-newElems)
			if(!elList.contains(ne)) elList +=ne
			
		notifyListeners
	}
	
	def setSelection(newElems:Seq[GraphElem]) = {
		elList.clear
		elList ++=newElems
		notifyListeners
	}
	
	def registerSelectListener(listener:SelectListener)= {
		listeners+=listener
	}
	
	def elemRemoved(elem:GraphElem) = {
		val ix=list.indexOf(elem)
		if(ix> -1) {
			list.remove(ix)
			if(list.size==0)notifyListeners
		}
	}
	
	def elemChanged(oldEl:GraphElem,newEl:GraphElem)={
		val ix=list.indexOf(oldEl)
		if(ix> -1) list(ix)=newEl
	}
	
	
	
	def notifyListeners= listeners.foreach(_.selectionChanged(this,elList))
	
	

}