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
import java.awt.event.{MouseAdapter,MouseWheelListener,MouseWheelEvent}
import client.dataviewer.sidePanel.SidePanelController

/** controls the DataViewer
 *  manages the general loading of data
 * 
 */


class DataViewController  extends PathControllable with SelectSender with Referencable  {
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
	
	private var superScrollPane:ScrollPane=null
	
	val panel=new BoxPanel(Orientation.Vertical)  {		
		override lazy val peer = new javax.swing.JPanel with SuperMixin with javax.swing.Scrollable {       
    val l = new javax.swing.BoxLayout(this, Orientation.Vertical.id)
    setLayout(l)
    val mSize=new Dimension(Short.MaxValue,Short.MaxValue)
    def getPreferredScrollableViewportSize: Dimension=getPreferredSize 
    override def getPreferredSize = { 
    	val ps=super.getPreferredSize
    	
    	if(superScrollPane!=null) {
    		val scSize:Dimension=(superScrollPane.peer).getVerticalScrollBar.getSize
    		val scrollbarWidth=scSize.width+8
    		val hAmount=(if(superScrollPane.peer.getVerticalScrollBar.isVisible)scrollbarWidth else 8)
    		val vAmount=(if(superScrollPane.peer.getHorizontalScrollBar.isVisible)scrollbarWidth else 8)
    		//System.out.println("dataView prefSize:"+ps+ " super:"+superScrollPane.size+" scrollWidth:"+scrollbarWidth)
    		new Dimension(
    		if( ps.width<superScrollPane.size.width-hAmount) superScrollPane.size.width-hAmount else ps.width ,
    		if( ps.height<superScrollPane.size.height-vAmount) superScrollPane.size.height-vAmount else ps.height )	
    	}
    		
    	else ps
    }
    
    override def getMaximumSize=mSize
    
  
    def getScrollableTracksViewportHeight: Boolean =false
    def getScrollableTracksViewportWidth: Boolean=false
  
    def getScrollableBlockIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int = 200  
    def getScrollableUnitIncrement(visibleRect: Rectangle, orientation: Int, direction: Int): Int= 10    
  }
		
	}
	
	var selectedInstance: InstanceData= _ // to be deleted
	
	var openChildCallBack:(Reference)=> Unit = _
	var selectListener= collection.mutable.HashSet[SelectListener]()
	
	var containerFocusListener= collection.mutable.HashSet[ContainerFocusListener]()
	
	//var scrollEventListener:ScrollEventListener=null
	
	
	//val vGlue=Swing.VGlue
	/** is called by PathModel when another instance should be loaded
	 *  @param parentRef the new Instance to be loaded
	 *  @param selectRef reference of an instance that should be selected
	 */
	def openData(nparentRef:Reference,selectRef:Option[Reference]) = {
		//System.out.println("open Data: "+nparentRef+" "+loaded)
	  if(loaded) shutDown()
	  if(!currentSidePanelControllerWasUsed)currentSidePanelController=None
	  else currentSidePanelControllerWasUsed=false
	  selectedInstance=null
	  ref=nparentRef	  
	  mainClass=AllClasses.get.getClassByID(ref.typ)
	  val hiddenFields=mainClass.getNum_FirstHiddenPropFields
	  println("class:"+mainClass.name+" hiddenFields:"+hiddenFields)
	  
	  for(i <- 0 until mainClass.propFields.size) {
	  	val propFieldInfo=mainClass.propFields(i)
	  	if(!(propFieldInfo.hidden && DataViewController.hideProperties)) {
	  		val mod=getPropModel
	  		panel.contents+=mod.panel	  	
	  		mod.load(propFieldInfo.allowedClass,i.toByte,propFieldInfo.name,selectRef,i==hiddenFields,mainClass.propFields.size==1)	  	
	  	}	  	
	  }
	  //panel.contents+=vGlue
	  updateHeight()
	  if(!selectRef.isDefined) selectListener foreach(_.selectionChanged(this,EMPTY_GROUP.list ))
	  loaded =true
	}
	
	def setSuperScrollPane(sc:ScrollPane)= superScrollPane=sc
	
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
		javax.swing.SwingUtilities.invokeLater(new Runnable(){
			def run= {
			  //panel.preferredSize=new Dimension(getWidth,getHeight)		
			  panel.revalidate
			  panel.repaint	
			}
		})	
	}
	
	/*def getHeight=propertyModels.take(numUsedModels).foldRight(0){(n,result)=> result+n.getHeight}
	
	def getWidth=propertyModels.take(numUsedModels).foldRight(0){(n,result)=> if(n.getWidth>result) n.getWidth else result}*/
	
	
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
		panel.contents.clear
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
	
	/*def handleScrollEvent(e:MouseWheelEvent )= {
		if(scrollEventListener!=null) scrollEventListener.handleScrollEvent(e)
	}*/

}

object DataViewController {
	var hideProperties:Boolean=false
}

/*trait ScrollEventListener {
	def handleScrollEvent(e:MouseWheelEvent)
}*/