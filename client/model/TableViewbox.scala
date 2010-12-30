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
	val pathView=new ListView[InstanceData]()
	var viewbox:Viewbox=null
	val openPathListener = new PathControllable {
		def openData(parentRef:Reference,selectRef:Option[Reference])= 
			viewbox.setTitle( pathMod.getTitleText)
		def registerOpenChildCallBack(callBack: (Reference)=> Unit) = {}
	}
	val pathController=new PathController(pathMod,pathView,List(dataviewController,openPathListener))
	var pathFactoryIndex= -1
	var pathBoxOpen=true
	val emptyBox=Swing.HStrut(0)
	
	//val pathLabel=new Label()
	
	border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
  val pathScroller =new ScrollPane()  {
  	   peer.setWheelScrollingEnabled(true)
				viewportView= pathView				
				preferredSize=new Dimension(200,200)
				def callback(nsize:Int):Unit= {
					//System.out.println(size+" "+pathView.peer.getFixedCellHeight())
					preferredSize=new Dimension(400,(nsize+1)*pathController.lineHeight )
					maximumSize=preferredSize
					TableViewbox.this.revalidate
					TableViewbox.this.peer.invalidate
				}
				pathController.registerSizeChangeListener(callback)
				
			}
  val switchPathButton=new Button("\u02c4")
	
	val pathBox=new BorderPanel{
		add(pathScroller,BorderPanel.Position.Center)
		add(new BoxPanel(Orientation.Vertical) {
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
  
  
  val tableScroller=new ScrollPane() /*with ScrollEventListener*/ {
  			//val mwls:Array[MouseWheelListener]=peer.getMouseWheelListeners()   			
				viewportView=dataviewController.panel
				peer.setWheelScrollingEnabled(true)
				//System.out.println("mwls:"+mwls.mkString(","))
				/*def handleScrollEvent(e:MouseWheelEvent)= {
					if(mwls.size>0){
						System.out.println(mwls(0).getClass)
						mwls(0).mouseWheelMoved( e)
					}
					
				}*/
			}
  
	add (tableScroller,BorderPanel.Position.Center)
	//dataviewController.scrollEventListener =tableScroller	
	dataviewController.setSuperScrollPane(tableScroller)
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
  		//switchPathButton .text="+"  		
  		add(emptyBox,BorderPanel.Position.North)
  		viewbox.setTitle(	pathMod.getTitleText)//(pathMod.dataList.get.last,pathMod.dataList.get.size-1)else "empty")
  		viewbox.minimizeHeaderPanel(switchPathBox)
  		//pathBox.setCenterComp(pathLabel)
  		revalidate
  	}else {
  		//switchPathButton .text="-" // open
  		add (pathBox,BorderPanel.Position.North)
  		//revalidate
  	}
  	pathBoxOpen= !pathBoxOpen
  }
  
  def typeID:String = "Table"
}

object TableViewbox {
	
	
}