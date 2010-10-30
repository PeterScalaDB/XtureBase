/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import client.dialog._
import scala.swing._
import scala.swing.event._
import definition.data._
/** manages the Layer panel
 * 
 */
class LayerPanelController(viewController:GraphViewController) extends SelectListener {
	
  val layerTable:Table=new Table() {
  	var lastColumn:Int= -1
  	var lastRow:Int= -1
  	
		peer.setModel(viewController.layerModel)
		autoResizeMode=Table.AutoResizeMode.Off
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.None
		val colMod=peer.getColumnModel()
		colMod.getColumn(0).setMinWidth(50)
		colMod.getColumn(0).setPreferredWidth(400)
		colMod.getColumn(1).setPreferredWidth(60)
		colMod.getColumn(2).setPreferredWidth(60)
		colMod.getColumn(3).setPreferredWidth(60)
		colMod.getColumn(4).setPreferredWidth(50)
		colMod.getColumn(5).setPreferredWidth(70)
		colMod.getColumn(6).setPreferredWidth(70)
		listenTo(mouse.clicks)
		reactions+={
			case e:MousePressed => {
				lastColumn= peer.columnAtPoint(e.point)
				lastRow=peer.rowAtPoint(e.point)
			}
			case e:MouseReleased => if(e.clicks ==1) {
				if(lastColumn==peer.columnAtPoint(e.point)&& lastColumn> -1&&
						lastRow==peer.rowAtPoint(e.point)&& lastRow> -1)
					tableCellClicked(lastColumn,lastRow)
			}
		}
	}
  
  def tableCellClicked(col:Int,row:Int)= {
  	if(col==0) {
  		if (row==viewController.layerModel.layerList.size) addLayer
  	}
  	else if(row<viewController.layerModel.layerList.size)
  	col match {  		 
  		case 1 => toggleVisible(row)
  		case 2 => toggleEdible(row)
  		case 3 => toggleActive(row)
  		case 4 => removeLayer(row) 
  		case _ =>
  	}
  }
  
  // instances from other models
  var selectedInstances:Seq[Referencable]=Seq.empty
  
  val addBut =new Button("Hinzufügen")
	val removeBut =new Button("Entfernen")
	val activateBut =new Button("Aktivieren")
	val visibleBut =new Button("Sichtbar")
	val edibleBut:Button =new Button("Veränderbar")
  
  val layerPanel:BorderPanel = new BorderPanel() {
  	
		add(new ScrollPane() {
			viewportView= layerTable
			preferredSize=new Dimension(100,100)
		},BorderPanel.Position.Center)
		/*add(new GridPanel(1,5){
			
			//contents+=addBut+=removeBut+=visibleBut+=edibleBut+=activateBut
			/*listenTo(addBut,removeBut,visibleBut,edibleBut,activateBut)
			reactions+= {
				case ButtonClicked(`addBut`) => addLayer
				//case ButtonClicked(`removeBut`) => removeLayer
				/*case ButtonClicked(`visibleBut`) => toggleVisible
				case ButtonClicked(`edibleBut`) => toggleEdible
				case ButtonClicked(`activateBut`) => toggleActive*/
			}*/
		},BorderPanel.Position.South)*/
	}
  
  def selectionChanged(sender:SelectSender,instList:Seq[Referencable])= {
		selectedInstances=instList
		viewController.layerModel.setCanLoadElements(canLoad)		
	}
  
  def canLoad= selectedInstances!=null &&(!selectedInstances.isEmpty) && selectedInstances.head.ref.typ == Layer.displayListTyp &&
				 selectedInstances.head.isInstanceOf[InstanceData]&& !viewController.layerModel.containsRef(selectedInstances.head.ref)
  
  def addLayer = {
		//println("sel:"+selectedInstances+" ref:"+selectedInstances.head.ref+" "+Layer.displayListTyp)
		if(canLoad) {	
			for(aLayer <-selectedInstances;if (aLayer.ref.typ == Layer.displayListTyp && 
					aLayer.isInstanceOf[InstanceData]&& !viewController.layerModel.containsRef(aLayer.ref) )) {
			  val newLayer=Layer.createLayer(viewController,aLayer.asInstanceOf[InstanceData])
			  viewController.layerModel.addLayer( newLayer)
			  //updateProperty
			  newLayer.load	
			}			
		}
	}
  
  def updateProperty = {  	
  	//layerPanel.peer.putClientProperty("newPanel", if(viewController.layerModel.hasVisibleLayers) viewController.newPanel else null)
  	viewController.canvas.repaint
  }
  	 
  
  def getSelectedLayer:Int = if ( layerTable.selection.rows.isEmpty) -1 else layerTable.selection.rows.head 
	
	def removeLayer(ix:Int) = {
		 //val ix=getSelectedLayer 
		 if(ix> -1) {
			 viewController.layerModel.layerList(ix).shutDown
			 viewController.layerModel.removeLayer(ix)
			 //updateProperty
			 //TestGraphListModel.update()
		 }
		 viewController.selectModel.deselect(true)
	}
  
  def toggleVisible(ix:Int) = {		 
		if(ix> -1)  viewController.layerModel.toggleVisibility(ix)	
		updateProperty
		
	}
  
  def toggleEdible(ix:Int) = { 
		if(ix> -1)  viewController.layerModel.toggleEdible(ix)	
		updateProperty
	}
  
  def toggleActive(ix:Int) = {		 
		if(ix> -1)  viewController.layerModel.toggleActive(ix)
		updateProperty
	}
  
}