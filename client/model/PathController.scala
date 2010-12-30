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
import java.awt.{Dimension,Color}
import client.dataviewer.DataViewController
import client.dialog.ContainerFocusListener

/** manages the connection of a PathModel and a ListView
 * 
 */
class PathController (val model:PathModel, val view:ListView[InstanceData],val listener:Seq[PathControllable]) {
	
	class MyRenderer extends BoxPanel(Orientation.Horizontal ) {
		val firstLabel=new Label
		val resultLabel=new Label
		//var dataIsSelected=false
		contents+=firstLabel+=Swing.HGlue+=resultLabel+=Swing.HStrut(10)
		
		
		
		override def foreground_=(c: Color) = {			 
			firstLabel.foreground= c
			resultLabel.foreground=c
		}
		
		def config( isSelected: Boolean, focused: Boolean, a: InstanceData, index: Int) {
       //or whatever			
			opaque=true
			//firstLabel.opaque=true       
  	  firstLabel.text=getLabelText(a,index)
  	  firstLabel.horizontalAlignment=Alignment.Left
  	  resultLabel.text=a.resultString
		}
	}
	
		//preferredSize=new Dimension(50,20)
		
	def lineHeight=24

	
	var oldIndex= -1
	
	val renderPrototype=new MyRenderer
	var sizeChangeListeners= collection.mutable.HashSet[(Int)=>Unit]()
	@scala.volatile var updating=false
	listener.foreach (_.registerOpenChildCallBack(openChildFromListener))
	view.focusable=false
	view.peer.setModel(model)
	view.peer.setDragEnabled(true)
	view.selection.intervalMode=ListView.IntervalMode.Single
	
	view.fixedCellHeight=lineHeight
	view.background=new Color(225,225,230)
	view.selectionForeground=new Color(0,0,40)
	view.selectionBackground=new Color(210,210,215)
	
	view.listenTo(view.selection)
	view.renderer=new ListView.AbstractRenderer[InstanceData,MyRenderer](renderPrototype){
		def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: InstanceData, index: Int) {
			component.config(isSelected,focused,a,index)
		}
		
	}
	view.reactions += {
		case ListSelectionChanged(list,range,live) => {			
			if (!live&& !view.selection.indices.isEmpty) selectionChanged(view.selection.indices.first)			
		}		
	}
	
	
	def getLabelText(a:InstanceData,index:Int):String = {
		val prefix=if(index==0) "\u252c"
  		else if (index==model.dataList.get.size-1) "\u2514\u2500" 
  		else "\u2514\u252c"
  	val indent="    " * (if(index==0) 0 else (index-1))
  	indent+prefix+" "+(if(a!=null) a.toString else "") 
	}
	
	
	
	def selectionChanged(newPos:Int)= {		
		if (!updating &&  (newPos!=oldIndex) && (newPos < model.getSize) ) {
			// change Subscription only to the remaining elements
			val selectRef:Option[Reference] = // what element to select in the table
			if(newPos<oldIndex) {
				if((newPos+1) >=model.getSize) None
				else {
					val ret=Some(model.getInstanceAt(newPos+1).ref) // select last pos below newPos
					model.jumpUp(newPos)
					ret
				}
			} else None
			oldIndex=newPos
			// notify listener
			listener.foreach (_.openData(model.getInstanceAt(newPos).ref,selectRef))
		}
		if (updating) {			
			if(newPos!=model.getSize-1) {
				System.out.println("wrong selection")				
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
		listener.foreach(_.openData(newPath.last,None))
		notifySizeListeners()
	}
	
	def selectLastLine():Unit = {
		//System.out.println("select LL size:"+model.getSize+" subsID:"+model.subsID+" "+updating+ " "+Thread.currentThread)
		if(updating) {
 		  SwingUtilities.invokeLater(new Runnable {
				def run () = {
					 //System.out.println("Size is:"+model.getSize)
					view.selectIndices(model.getSize-1)
					
					}	})		
				
			//updating=false	
		}
		
	}
	
	def addPathElement(newElement:Reference) = {
		updating=true
		model.addPathElement(newElement)		
		listener.foreach(_.openData(newElement,None))
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