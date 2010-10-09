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
	
  val layerTable=new Table() {
		peer.setModel(viewController.layerModel)
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.Row
	}
  
  // instances from other models
  var selectedInstances:Seq[Referencable]=Seq.empty
  
  val addBut =new Button("Hinzufügen")
	val removeBut =new Button("Entfernen")
	val activateBut =new Button("Aktivieren")
	val visibleBut =new Button("Sichtbar")
	val edibleBut =new Button("Veränderbar")
  
  val layerPanel = new BorderPanel() {
		add(new ScrollPane() {
			viewportView= layerTable
			preferredSize=new Dimension(100,100)
		},BorderPanel.Position.Center)
		add(new GridPanel(1,5){
			
			contents+=addBut+=removeBut+=visibleBut+=edibleBut+=activateBut
			listenTo(addBut,removeBut,visibleBut,edibleBut,activateBut)
			reactions+= {
				case ButtonClicked(`addBut`) => addLayer
				case ButtonClicked(`removeBut`) => removeLayer
				case ButtonClicked(`visibleBut`) => toggleVisible
			}
		},BorderPanel.Position.South)
	}
  
  def selectionChanged(sender:SelectSender,instList:Seq[Referencable])= {
		selectedInstances=instList
		addBut.enabled=canLoad
	}
  
  def canLoad= selectedInstances!=null &&(!selectedInstances.isEmpty) && selectedInstances.head.ref.typ == Layer.displayListTyp &&
				 selectedInstances.head.isInstanceOf[InstanceData]&& !viewController.layerModel.containsRef(selectedInstances.head.ref)
  
  def addLayer = {
		//println("sel:"+selectedInstances+" ref:"+selectedInstances.head.ref+" "+Layer.displayListTyp)
		if(canLoad) {			
			val newLayer=Layer.createLayer(viewController,selectedInstances.head.asInstanceOf[InstanceData])
			viewController.layerModel.addLayer( newLayer)
			newLayer.load
			
		}
	}
  
  def getSelectedLayer:Int = if ( layerTable.selection.rows.isEmpty) -1 else layerTable.selection.rows.head 
	
	def removeLayer = {
		 val ix=getSelectedLayer 
		 if(ix> -1) {
			 viewController.layerModel.layerList(ix).shutDown
			 viewController.layerModel.removeLayer(ix)
			 TestGraphListModel.update()
		 }
		 viewController.selectModel.deselect(true)
	}
  
  def toggleVisible = {
		val ix=getSelectedLayer 
		if(ix> -1)  viewController.layerModel.toggleVisibility(ix)	 
	}
  
}