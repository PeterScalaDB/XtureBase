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

/**
 * 
 */
class TableViewbox extends BorderPanel with ViewboxContent {
	
	val dataviewController=new DataViewController()
	val pathMod=new PathModel()
	val pathView=new ListView[InstanceData]()
	var viewbox:Viewbox=null
	val openPathListener = new PathControllable {
		def openData(parentRef:Reference,selectRef:Option[Reference])= 
			viewbox.setTitle("Tabelle "+ (if(pathMod.dataList.isDefined) pathMod.dataList.get.last else "[leer]"))
		def registerOpenChildCallBack(callBack: (Reference)=> Unit) = {}
	}
	val pathController=new PathController(pathMod,pathView,List(dataviewController,openPathListener))
	var pathFactoryIndex= -1
	var pathBoxOpen=true
	
	val pathLabel=new Label()
	
	border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
  val pathScroller =new ScrollPane() {
				viewportView= pathView				
				preferredSize=new Dimension(200,200)
				def callback(nsize:Int):Unit= {
					//println(size+" "+pathView.peer.getFixedCellHeight())
					preferredSize=new Dimension(400,(nsize+1)*pathController.lineHeight )
					maximumSize=preferredSize
					TableViewbox.this.revalidate
					TableViewbox.this.peer.invalidate
				}
				pathController.registerSizeChangeListener(callback)
			}
  val switchPathButton=new Button("-")
	
	val pathBox=new BorderPanel{
		add(pathScroller,BorderPanel.Position.Center)
		add(new BoxPanel(Orientation.Vertical) {
			contents+= Swing.VGlue += switchPathButton
		},BorderPanel.Position.East)
		listenTo(switchPathButton)
		reactions += {
			case ButtonClicked(`switchPathButton`) => {
				switchPathBox()
			}
		}
		def setCenterComp(comp:Component) = add(comp,BorderPanel.Position.Center)
	}
	pathLabel.horizontalAlignment=Alignment.Left
  switchPathButton.margin=new Insets(0,0,0,0)
	switchPathButton.focusable=false
  preferredSize=new Dimension(400,pathController.lineHeight )
  dataviewController.registerSelectListener(SelectEventDispatcher)
  dataviewController.registerContainerFocusListener(NewPanelArea)
  add (pathBox,BorderPanel.Position.North)
	add (new ScrollPane() {
				viewportView=dataviewController.panel
			},BorderPanel.Position.Center)
  //preferredSize=new Dimension(100,100)
			
	def open(): Unit = {  	
  	val entry=PathFactory.getNextPathEntry
  	//println("Open "+entry)
  	pathFactoryIndex=entry._1
  	pathController.loadPath(entry._2)  
  	}

  def close(): Unit = {
  	if(pathController.model.dataList.isDefined)
  	PathFactory.releasePathEntry(pathFactoryIndex, pathController.model.dataList.get.map(_.ref))
  	shutDown
  }
  
  def shutDown() = {
  	pathMod.shutDown
  	dataviewController.shutDown
  }

  def getType(): ViewboxContentType = { TableViewbox.tableBoxType }
  
  def setViewbox(newbox:Viewbox)= viewbox=newbox
  
  def switchPathBox():Unit = {
  	if(pathBoxOpen) { // close
  		switchPathButton .text="+"
  		pathLabel.text=if (pathMod.dataList.isDefined) 
  			pathController.getLabelText(pathMod.dataList.get.last,pathMod.dataList.get.size-1)
  			else "empty"
  		pathBox.setCenterComp(pathLabel)
  		pathBox.revalidate
  	}else {
  		switchPathButton .text="-" // open
  		pathBox.setCenterComp(pathScroller)
  		pathBox.revalidate
  	}
  	pathBoxOpen= !pathBoxOpen
  }
}

object TableViewbox {
	val tableBoxType=new ViewboxContentType(1,"table","T",()=>{
		new TableViewbox
	})
	
}