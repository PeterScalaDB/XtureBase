/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import definition.data._
import scala.swing._
import scala.swing.event._
import scala.collection.immutable._
import javax.swing.SwingUtilities
import client.comm.{ClientQueryManager,UserSettings}

/** manages the connection of a PathModel and a ListView
 * 
 */
class PathController (val model:PathModel, val view:ListView[InstanceData],val listener:PathControllable) {
	var oldIndex= -1
	
	var sizeChangeListeners= collection.mutable.HashSet[(Int)=>Unit]()
	@scala.volatile var updating=false
	listener.registerOpenChildCallBack(openChildFromListener)
	view.peer.setModel(model)
	view.selection.intervalMode=ListView.IntervalMode.Single
	view.listenTo(view.selection)
	view.reactions += {
		case ListSelectionChanged(list,range,live) => {			
			if (!live&& !view.selection.indices.isEmpty) selectionChanged(view.selection.indices.first)			
		}		
	}
	
	ClientQueryManager.registerSetupListener(() => {
		val p=UserSettings.getListProperty("pathController","currentPath",Seq(Reference(10,1)))
		println("loading path "+p)
		loadPath(p)
	})
	ClientQueryManager.registerStoreSettingsListener(() => {
		UserSettings.setListProperty("pathController","currentPath",model.dataList match {
			case Some(list) => list.map(_.ref).asInstanceOf[collection.immutable.Seq[Reference]]
			case None => Seq(Reference(10,1))
		})		
	})
	
	
	def selectionChanged(newPos:Int)= {		
		if (!updating &&  (newPos!=oldIndex) && (newPos < model.getSize) ) {
			// change Subscription only to the remaining elements
			val selectRef:Option[Reference] = 
			if(newPos<oldIndex) {
				val ret=Some(model.getInstanceAt(newPos+1).ref) // select last pos below newPos
				model.jumpUp(newPos)
				ret
			} else None
			oldIndex=newPos
			// notify listener
			listener.openData(model.getInstanceAt(newPos).ref,selectRef)
		}
		if (updating) {			
			if(newPos!=model.getSize-1) {
				println("wrong selection")				
				//view.selectIndices(model.getSize-1)			
			}
			oldIndex=newPos
			updating=false		
		}
		notifySizeListeners()
	}
	
	def loadPath(newPath:Seq[Reference]) = {
		updating=true
		model.loadPath(newPath)(selectLastLine)
		//view.selectIndices( newPath.size-1)		
		listener.openData(newPath.last,None)
		notifySizeListeners()
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
		listener.openData(newElement,None)
		notifySizeListeners()
	}
	
	// callback routine to be called from the listener
	def openChildFromListener(ref:Reference) = addPathElement(ref)
	
         
	def shutDown() = {
		model.shutDown()
	}
	
	def registerSizeChangeListener(func:(Int)=>Unit) = sizeChangeListeners += func
	
	private def notifySizeListeners() = {
		val size=model.getSize
		for(func <-sizeChangeListeners) func(size)
	}
}