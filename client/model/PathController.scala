/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import definition.data._
import scala.swing._
import scala.swing.event._
import scala.collection.immutable._
import javax.swing.SwingUtilities

/** manages the connection of a PathModel and a ListView
 * 
 */
class PathController (val model:PathModel, val view:ListView[InstanceData],val listener:PathControllable) {
	var oldIndex= -1
	@scala.volatile var updating=false
	view.peer.setModel(model)
	view.selection.intervalMode=ListView.IntervalMode.Single
	view.listenTo(view.selection)
	view.reactions += {
		case ListSelectionChanged(list,range,live) => { 
			//println("Range:"+range+" lead:"+view.selection.leadIndex+" anch:"+view.selection.anchorIndex);
			
			if (!live&& !view.selection.indices.isEmpty) selectionChanged(view.selection.indices.first)
			//println("#####################################")
			//Thread.dumpStack
		}
		
	}
	
	def selectionChanged(newPos:Int)= {
		//println(" sel changed newpos:"+newPos+" oldIx:"+oldIndex+" model size:"+model.getSize+" updating:"+updating+" "+Thread.currentThread)
		//println(" indices:"+view.selection.indices)
		if (!updating &&  (newPos!=oldIndex) && (newPos < model.getSize) ) {
			// change Subscription only to the remaining elements
			if(newPos<oldIndex)			
			model.jumpUp(newPos)
			oldIndex=newPos
			// notify listener
			listener.openData(model.getInstanceAt(newPos).ref)
		}
		if (updating) {			
			if(newPos!=model.getSize-1) {
				println("wrong selection")				
				//view.selectIndices(model.getSize-1)			
			}
			oldIndex=newPos
			updating=false		
		}
	}
	
	def loadPath(newPath:IndexedSeq[Reference]) = {
		updating=true
		model.loadPath(newPath)(selectLastLine)
		//view.selectIndices( newPath.size-1)		
		listener.openData(newPath.last)
	}
	
	def selectLastLine():Unit = {
		//println("select LL size:"+model.getSize+" subsID:"+model.subsID+" "+updating+ " "+Thread.currentThread)
		if(updating) {
 		  SwingUtilities.invokeLater(new Runnable {
				def run () = {
					 //println("Size is:"+model.getSize)
					view.selectIndices(model.getSize-1)
					
					}	})		
				
			//updating=false	
		}
		
	}
	
	def addPathElement(newElement:Reference) = {
		updating=true
		model.addPathElement(newElement)		
		listener.openData(newElement)
	}
         
	def shutDown() = {
		model.shutDown()
	}
}