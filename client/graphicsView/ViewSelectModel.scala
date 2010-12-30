/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import client.dialog._
import collection.mutable.ArrayBuffer
import definition.typ.SelectGroup

/**
 * 
 */
class ViewSelectModel(controller:GraphViewController) extends SelectSender {
	private val elMap=collection.mutable.HashMap[Layer,SelectGroup[GraphElem]]()
	//private val elList=collection.mutable.ArrayBuffer[GraphElem]()
	
	private val listeners=collection.mutable.HashSet[SelectListener]()
	
	def list=elMap.valuesIterator
	
	def deselect(notify:Boolean)= {
		elMap.clear
		controller.stopModus()
		controller.canvas.repaint
		if(notify) notifyListeners
	}
	
	/** removes all elements from the given Layer from the selection
	 * 
	 * @param lay
	 */
	def deselectLayer(lay:Layer) = {
		elMap-=(lay)		
		notifyListeners	
	}
	
	/** adds the elements to the selection
	 * 
	 * @param lay new Elements are on wich layer
	 * @param newElems the elements to dd
	 * @param toggle should the elements be removed when they are already in the selection ?
	 */
	def addSelection(lay:Layer,newElems:Seq[GraphElem],toggle:Boolean) ={
		val elList=getElList(lay) 
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
	
	 
	
	def setSelection(lay:Layer,newElems:Seq[GraphElem]) = {
		System.out.println("set selection "+lay+" "+newElems)
		val elList=getElList(lay)
		elList.clear
		elList ++=newElems
		notifyListeners
	}
	
	private def getElList(lay:Layer)=if(elMap.contains(lay)) elMap(lay).children.asInstanceOf[ArrayBuffer[GraphElem]]
		else {
			val newList=new ArrayBuffer[GraphElem]
			elMap(lay)=new SelectGroup[GraphElem](lay.ownerRef,newList)
			newList
		}
	
	def registerSelectListener(listener:SelectListener)= {
		listeners+=listener
	}
	
	def elemRemoved(lay:Layer,elem:GraphElem) = {
		val elList=getElList(lay)
		val ix=elList.indexOf(elem)
		if(ix> -1) {
			elList.remove(ix)
			notifyListeners
		}
	}
	
	def elemChanged(lay:Layer,newEl:GraphElem)={
		val elList=getElList(lay)
		val ix=elList.findIndexOf(_.ref==newEl.ref)
		if(ix> -1) elList(ix)=newEl
	}
	
	def elementsChanged(lay:Layer,newElements:Seq[GraphElem])={
		val elList=getElList(lay)
		for(el<-newElements) {
		  val ix=elList.findIndexOf(_.ref==el.ref)
		  if(ix> -1) elList(ix)=el	
		}		
	}
	
	
	def notifyListeners= listeners.foreach(_.selectionChanged(this,elMap.valuesIterator.toSeq))
	
	

}