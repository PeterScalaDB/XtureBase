/**
 * Author: Peter Started:10.11.2010
 */
package client.model

import client.layout._
import scala.swing._
import definition.data._
import client.dataviewer._
import client.dialog._
import scala.swing.event.ButtonClicked
import javax.swing.BorderFactory
import javax.swing.border._
import java.awt.event.{MouseAdapter,MouseWheelListener,MouseWheelEvent}
import javax.swing.plaf.basic._
/**
 * 
 */
class TableViewbox extends BorderPanel with ViewboxContent  {
	
	val dataviewController=new DataViewController()
	
	val pathMod=new PathModel()
	val pathView=new ListView[InstanceData](){
		preferredSize=new Dimension(200,200)
				def callback(nsize:Int):Unit= {
					//System.out.println(size+" "+pathView.peer.getFixedCellHeight())
					preferredSize=new Dimension(400,(nsize)*pathController.lineHeight+2 )
					maximumSize=preferredSize
					TableViewbox.this.revalidate
					TableViewbox.this.peer.invalidate
				}				
	}
	
	var viewbox:Viewbox=null
	
	val openPathListener = new PathControllable {
		def openData(parentRef:Reference,selectRef:Option[Reference],ident:Int)= 
			viewbox.setTitle( pathMod.getTitleText)
		
			def registerOpenChildCallBack(callBack: (Reference)=> Unit) = {}		
	}
	
	val pathController:PathController=new PathController(pathMod,pathView,List(dataviewController))
	pathController.registerSizeChangeListener(pathView.callback)
	pathController.registerSizeChangeListener((a) => viewbox.setTitle(pathMod.getTitleText))
	var pathFactoryIndex= -1
	var pathBoxOpen=true
	val emptyBox=Swing.HStrut(0)
	
	dataviewController.setParentLineHeight(pathController.lineHeight)	
	
	border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
  
  val switchPathButton=new Button("\u02c4")
  switchPathButton.peer.putClientProperty("JComponent.sizeVariant", "small");
	switchPathButton.peer.updateUI()
	
	val pathBox=new BorderPanel{  	
		add(/*pathScroller*/pathView,BorderPanel.Position.Center)
		add(new BoxPanel(Orientation.Vertical) {
			preferredSize=new Dimension(36,20)
			contents+= Swing.VGlue += switchPathButton
		},BorderPanel.Position.West)
		listenTo(switchPathButton)
		reactions += {
			case ButtonClicked(`switchPathButton`) => {
				switchPathBox()
			}
		}
		//def setCenterComp(comp:Component) = add(comp,BorderPanel.Position.Center)
	}
	//pathLabel.horizontalAlignment=Alignment.Left
  switchPathButton.margin=new Insets(0,0,0,0)
	switchPathButton.focusable=false
  //preferredSize=new Dimension(400,pathController.lineHeight )
  dataviewController.registerSelectListener(SelectEventDispatcher)
  dataviewController.registerContainerFocusListener(NewPanelArea)
  add (pathBox,BorderPanel.Position.North)  
  
  
  
	add (dataviewController.splitter,BorderPanel.Position.Center)		
	//dataviewController.setSuperScrollPane(tableScroller)
  preferredSize=new Dimension(100,100)
			
	def open(): Unit = {  	
  	val entry=PathFactory.getNextPathEntry
  	//System.out.println("Open "+entry)
  	pathFactoryIndex=entry._1
  	pathController.loadPath(entry._2)  
  	}

  def close(): Unit = { shutDown }
  
  def storeSettings() = {
  	if(pathController.model.dataList.isDefined)
  	PathFactory.releasePathEntry(pathFactoryIndex, pathController.model.dataList.get.map(_.ref))
  	
  }
  
  def shutDown() = {
  	pathMod.shutDown
  	dataviewController.shutDown
  }

  //def getType(): ViewboxContentType = { TableViewbox.tableBoxType }
  
  def setViewbox(newbox:Viewbox)= viewbox=newbox
  
  def switchPathBox():Unit = {
  	if(pathBoxOpen) { // close  		  		
  		add(emptyBox,BorderPanel.Position.North)
  		viewbox.setTitle(	pathMod.getTitleText)//(pathMod.dataList.get.last,pathMod.dataList.get.size-1)else "empty")
  		viewbox.minimizeHeaderPanel(switchPathBox)  		
  		revalidate
  	} 
  	else add (pathBox,BorderPanel.Position.North)  	
  	pathBoxOpen= !pathBoxOpen
  }
  
  def typeID:String = "Table"
}

object TableViewbox {
	
	
}