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
//import server.test.SimpleProfiler


/** Renderer for a pathView line
 * 
 */
class PathLineRenderer(model:Option[PathModel]=None) extends BoxPanel(Orientation.Horizontal ) {
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
  	  resultLabel.text=if (a!=null)a.resultString else ""
		} 
		
		def getLabelText(a:InstanceData,index:Int):String = {
		val prefix=if(index==0) "\u252c"
  		else if (model.isDefined){
  			if(index==model.get.dataList.get.size-1) "\u2514\u2500"
  			else "\u2514\u252c"	
  		}  
  		else "\u2514\u2500"
  	val indent="    " * (if(index==0) 0 else (index-1))
  	indent+prefix+" "+(if(a!=null) a.toString else "") 
	}
	}


/** manages the connection of a PathModel and a ListView
 * 
 */
class PathController (val model:PathModel, val view:ListView[InstanceData],val listener:Seq[PathControllable]) {
	
	private var newPth:Seq[Reference] =Seq.empty
	private val lock=new Object
	def lineHeight=24	
	private var oldIndex= -1	
	private val renderPrototype=new PathLineRenderer(Some(model))
	private var sizeChangeListeners= collection.mutable.HashSet[(Int)=>Unit]()	
	//@scala.volatile var updating=false
	
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
	view.renderer=new ListView.AbstractRenderer[InstanceData,PathLineRenderer](renderPrototype){
		def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, a: InstanceData, index: Int) {
			component.config(isSelected,focused,a,index)
		}
		
	}
	view.reactions += {
		case ListSelectionChanged(list,range,live) => {			
			if (!live&& !view.selection.indices.isEmpty) selectionChanged(view.selection.indices.first)			
		}		
	}	
	
	def selectionChanged(newPos:Int)= lock.synchronized{
		println("selectionChanged: newPos:"+newPos+" oldIndex:"+oldIndex+ " model.getSize:"+model.getSize/*+" updating:"+updating*/)
		if (/*!updating &&*/  (newPos!=oldIndex) && (newPos <= model.getSize) ) {
			
			// change Subscription only to the remaining elements
			val selectRef:Option[Reference] = // what element to select in the table
			if(newPos<oldIndex) {
				if((newPos/*+1*/) >=model.getSize) None
				else {
					val ret=Some(model.getInstanceAt(newPos+1).ref) // select last pos below newPos
					model.jumpUp(newPos)
					ret
				}
			} else None
			println("openData selectRef:"+selectRef+" data:"+model.getInstanceAt(newPos))
			oldIndex=newPos
			// notify listener
			listener.foreach (_.openData(model.getInstanceAt(newPos).ref,selectRef,newPos))
		}		
		notifySizeListeners()
	}
	
	def loadPath(newPath:Seq[Reference]) = lock.synchronized{
		println("load Path:"+newPath)		
		newPth=newPath
		model.loadPath(newPath)(selectLastLine)
		listener.foreach(_.openData(newPth.last,None,newPth.size))		
	}
	
	private def selectLastLine():Unit = {		
		SwingUtilities.invokeLater(new Runnable {
			def run () = {			
				view.selectIndices(-1)
				oldIndex=model.getSize
				notifySizeListeners()
			}	})						
	}
	
	def addPathElement(newElement:Reference) = lock.synchronized{
		//updating=true
		println("add Path element "+newElement+" model.size:"+model.getSize+" oldIX:"+oldIndex)				
		oldIndex+=1
		listener.foreach(_.openData(newElement,None,oldIndex))
		model.addPathElement(newElement)
		notifySizeListeners()
	}
	
	// callback routine to be called from the listener
	def openChildFromListener(ref:Reference) = addPathElement(ref)
	
         
	def shutDown() = {
		model.shutDown()
	}
	
	def registerSizeChangeListener(func:(Int)=>Unit) = sizeChangeListeners += func
	
	private def notifySizeListeners() = {
		//SimpleProfiler.measure("notify SizeListeners ")
		val size=oldIndex//model.getSize
		for(func <-sizeChangeListeners) func(size)
	}	
	
}