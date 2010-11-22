/**
 * Author: Peter Started:10.11.2010
 */
package client.graphicsView

import client.layout._
import scala.swing._
import client.dialog.{SelectEventDispatcher,NewPanelArea}
import scala.swing.event.ButtonClicked
import javax.swing.BorderFactory
import javax.swing.border._
import javax.swing.event.{TableModelListener,TableModelEvent}



/** a viewbox for graphic content
 * 
 */
class GraphicsViewbox extends BorderPanel with ViewboxContent {
	
	val graphViewController:GraphViewController=new GraphViewController
	val layerPanController:LayerPanelController=new LayerPanelController(graphViewController)
	var boxOpen:Boolean=true
	//val layerLabel:Label=new Label()
	val emptyBox=Swing.HStrut(0)
	var viewbox:Viewbox=null
	
	val layerScrollPane:ScrollPane = new ScrollPane() {
			//preferredSize=new Dimension(200,200)
			viewportView=layerPanController.layerTable
			graphViewController.layerModel.registerSizeChangeListener(callback)
			def callback(nsize:Int) = {
				preferredSize=new Dimension(10,(nsize+3)*LayerPanelController.lineHeight)
				maximumSize=preferredSize
				GraphicsViewbox.this.revalidate
				GraphicsViewbox.this.peer.invalidate
			}
		}
	border=BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
	
	graphViewController.layerModel.addTableModelListener(new TableModelListener (){
		def tableChanged(e:TableModelEvent) = {
			if(viewbox!=null)viewbox.setTitle(graphViewController.layerModel.getLabelText)
		}
	}
	)
	
	val switchLayerButton:Button=new Button("\u02c4")
	
	val layerBox=new BorderPanel{
		add(layerScrollPane,BorderPanel.Position.Center)
		add(new BoxPanel(Orientation.Vertical) {
			contents+= Swing.VGlue += switchLayerButton
		},BorderPanel.Position.West)
		listenTo(switchLayerButton)
		reactions += {
			case ButtonClicked(`switchLayerButton`) => {
				switchBox()
			}
		}
		//def setCenterComp(comp:Component) = add(comp,BorderPanel.Position.Center)
		
	}
	
	//preferredSize=new Dimension(400,200)
	add(graphViewController.canvasPanel,BorderPanel.Position.Center)
	add(layerBox,BorderPanel.Position.North)
	graphViewController.selectModel.registerSelectListener(SelectEventDispatcher)
	graphViewController.registerContainerListener(NewPanelArea)
	SelectEventDispatcher.registerSelectListener(layerPanController)
	//layerLabel.horizontalAlignment=Alignment.Left
  switchLayerButton.margin=new Insets(0,0,0,0)
	switchLayerButton.focusable=false
	
  def open(): Unit = { 
		
	}

  def close(): Unit = { 
  	shutDown()
  }
  def storeSettings() = {
  	
  }

  def getType(): ViewboxContentType = { null }

  def setViewbox(box: Viewbox): Unit = { viewbox=box }

  def shutDown() = {
  	graphViewController.layerModel.removeAllLayers()
  	SelectEventDispatcher.removeSelectListener(layerPanController)
  }
  
  def switchBox():Unit = {
  	if(boxOpen) { // close  		
  		
  		viewbox.setTitle(graphViewController.layerModel.getLabelText)	
  		add(emptyBox,BorderPanel.Position.North)
  		viewbox.minimizeHeaderPanel(switchBox)
  		revalidate
  	}else {  		
  		add(layerBox,BorderPanel.Position.North)
  		revalidate
  	}
  	boxOpen= !boxOpen
  }
  def typeID:String = "Graphics"
}