/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import client.model._
import definition.data._
import definition.typ._
import client.dialog._
import scala.swing._
import java.awt.Color
import definition.typ.form.FormBox
import java.awt.event.{MouseAdapter,MouseWheelListener,MouseWheelEvent}
import client.dataviewer.sidePanel.SidePanelController
import javax.swing.BorderFactory
import scala.swing.event.ButtonClicked
import client.comm.ClientQueryManager
//import server.test.SimpleProfiler

/** controls the DataViewer
 *  manages the general loading of data
 * 
 */


class DataViewController  extends PathControllable with SelectSender with Referencable  {
	//SimpleProfiler.dprint =true
	private var loaded=false
	var ref:Reference= _
	var mainClass:AbstractObjectClass = _
	
	var selGroup=new SelectGroup[InstanceData](null,Seq.empty)
	var selGroupList=List(selGroup)
	
	val smallFont=new Font("Arial",0,10)
	
	val propertyModels =scala.collection.mutable.ArrayBuffer[PropertyModel]()
	var numUsedModels=0
	
	var currentSidePanelController:Option[SidePanelController]=None
	var currentSidePanelControllerWasUsed:Boolean=false
	
	//private var superScrollPane:Option[ScrollPane]=None
	
	val formModel=new FormModel	
		
	val tablePanel=new BoxPanel(Orientation.Vertical)  {		
		override lazy val peer = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {       
			val l = new javax.swing.BoxLayout(this, Orientation.Vertical.id)
			setLayout(l)			
			def getPreferredScrollableViewportSize: Dimension=getPreferredSize
		  val mSize=new Dimension(Short.MaxValue,Short.MaxValue)
			/*override def getPreferredSize:Dimension = { 
				val ps=super.preferredSize    	
				for (sc <-superScrollPane) {
					val scSize:Dimension=(sc.peer).getVerticalScrollBar.getSize
					val scrollbarWidth=scSize.width+8
					val hAmount=(if(sc.peer.getVerticalScrollBar.isVisible)scrollbarWidth else 8)
					val vAmount=(if(sc.peer.getHorizontalScrollBar.isVisible)scrollbarWidth else 8)
					return new  Dimension(
						if( ps.width<sc.size.width-hAmount) sc.size.width-hAmount else ps.width ,
						if( ps.height<sc.size.height-vAmount) sc.size.height-vAmount else ps.height ) 
				//return ps
				}

				return ps
			}*/
			override def getMaximumSize=mSize
			override def getMinimumSize=getPreferredSize
			def getScrollableTracksViewportHeight: Boolean =false
			def getScrollableTracksViewportWidth: Boolean=false
			def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
			def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10    
		}		
	}
	
	val formPanel= new ParentPanel()
	
	/*val splitter:SplitPane=new SplitPane(Orientation.Horizontal ,formPanel,tablePanel){
		oneTouchExpandable=true
		//resizeWeight=1
		dividerSize=12
	}*/
	val tableScroller=new ScrollPane()  {  			   			
  	viewportView=tablePanel
  	peer.setWheelScrollingEnabled(true)				
  }  
	
	
	val splitter= new SplitPane(Orientation.Horizontal ,formPanel,tableScroller) {
		/*override lazy val peer = new javax.swing.JSplitPane(Orientation.Horizontal.id, formPanel.peer, 
			tablePanel.peer) with SuperMixin with javax.swing.Scrollable {
			def getPreferredScrollableViewportSize: Dimension=getPreferredSize
			def getScrollableTracksViewportHeight: Boolean =false
			def getScrollableTracksViewportWidth: Boolean=false
			def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
			def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10
		}*/
		maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
		oneTouchExpandable=true
		dividerSize=12
	}
	
	formPanel.splitter=splitter
	
	var selectedInstance: InstanceData= _ // to be deleted	
	var openChildCallBack:(Reference)=> Unit = _
	var selectListener= collection.mutable.HashSet[SelectListener]()	
	var containerFocusListener= collection.mutable.HashSet[ContainerFocusListener]()	
	
	/** is called by PathModel when another instance should be loaded
	 *  @param parentRef the new Instance to be loaded
	 *  @param selectRef reference of an instance that should be selected
	 */
	def openData(nparentRef:Reference,selectRef:Option[Reference],indent:Int) = {
		System.out.println("open Data: "+nparentRef+" "+loaded+" indent:"+indent)
		//SimpleProfiler.startMeasure("Open DataView "+nparentRef)
	  if(loaded) shutDown()
	  if(!currentSidePanelControllerWasUsed)currentSidePanelController=None
	  else currentSidePanelControllerWasUsed=false
	  selectedInstance=null
	  ref=nparentRef	  
	  //SimpleProfiler.measure("clone")
	  mainClass=AllClasses.get.getClassByID(ref.typ)	  
	  val newForm=mainClass.formBox match {
	  	case Some(f)=>Some(f.makeCopy)
	  	case _=> None
	  }	  
		//SimpleProfiler.measure("load")
	  formPanel.setForms(newForm,indent,nparentRef)
	  formModel.setForms(mainClass,newForm)
	  formModel.loadData(nparentRef)
	  val hiddenFields=mainClass.getNum_FirstHiddenPropFields
	  	  
	  for(i <- 0 until mainClass.propFields.size) {
	  	val propFieldInfo=mainClass.propFields(i)
	  	if(!(propFieldInfo.hidden && DataViewController.hideProperties)) {
	  		val mod=getPropModel
	  		tablePanel.contents+=mod.panel	  	
	  		mod.load(propFieldInfo.allowedClass,i.toByte,propFieldInfo.name,selectRef,i==hiddenFields,mainClass.propFields.size==1)	  	
	  	}	  	
	  }
		//SimpleProfiler.measure("loaded send")
	  //panel.contents+=vGlue
	  //updateHeight()
	  if(!selectRef.isDefined) selectListener foreach(_.selectionChanged(this,EMPTY_GROUP.list ))
	  //SimpleProfiler.measure("selectListener send")
	  loaded =true
	}
	
	/*def setSuperScrollPane(sc:ScrollPane)= {
		superScrollPane=Some(sc)		
	}*/
	
	def registerOpenChildCallBack(callBack: (Reference)=> Unit) = {
		openChildCallBack=callBack
	}
	
	def registerSelectListener(listener:SelectListener) = {
		selectListener += listener
	}
	
	def registerContainerFocusListener(listener:ContainerFocusListener) = {
		containerFocusListener += listener
	} 
	
	
	def updateHeight() = {
		//SimpleProfiler.measure("update Height")
		javax.swing.SwingUtilities.invokeLater(new Runnable(){
			def run= {
			  //panel.preferredSize=new Dimension(getWidth,getHeight)		
			  tablePanel.revalidate			  
			  splitter.peer.resetToPreferredSizes
			  tablePanel.repaint	
			}
		})	
	}
	
		
	def getPropModel = {
		numUsedModels  += 1
		if(propertyModels.size>=numUsedModels) propertyModels(numUsedModels-1)		
		else {
			val newMod= new PropertyModel(this)
			propertyModels append newMod
			newMod
		}
	}
	
	
	def shutDown() = {
		formModel.shutDown
		tablePanel.contents.clear
		updateHeight()
		for(i <-0 until numUsedModels) propertyModels(i).shutDown()
		// save the models for later use		
		numUsedModels=0		
		loaded=false		
	}
	
	def selectionChanged(tabMod:TypeTableModel,proMod:PropertyModel,instList:Seq[InstanceData]):Unit = {
		//System.out.println("sel: propfield:"+proMod.propertyField+" typ:"+tabMod.typ +" \n"+instList.mkString)
		selGroup.parent =proMod.ownerRef
		selGroup.children =instList
		for(i <- 0 until numUsedModels;val mod=propertyModels(i))
			mod.deselect(if(proMod==mod) tabMod.typ else -1)
		 selectListener foreach(_.selectionChanged(this,selGroupList))	
		//selectedInstance=inst
	}
	
	def containerFocused(currPropertyField:Int):Unit = {
		containerFocusListener foreach (_.containerFocused(this,currPropertyField,""))
	}
	
	def deselect(notify:Boolean) = {
		for(i <- 0 until numUsedModels;val mod=propertyModels(i))
			mod.deselect(-1)
		for(cfl <- containerFocusListener;if(cfl.alsoDeselect))
			cfl.containerFocused(null,0)
	}
	
	/** sends a message to the path controller that it should open a child instance
	 * 
	 * @param ref the reference of the child instance
	 */
	def openChild(ref:Reference) = {
		if(openChildCallBack!=null) openChildCallBack(ref)
	}	
	
	def setParentLineHeight(nheight:Int)= formPanel.setParentHeight(nheight)
	
}

object DataViewController {
	var hideProperties:Boolean=false
}
