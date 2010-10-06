/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import scala.swing._
import scala.swing.event._
import client.dataviewer.SelectListener
import definition.data._
import client.comm.ClientQueryManager
import javax.swing.SwingUtilities
import java.awt.event._

/**
 * 
 */
class GraphViewController() extends SelectListener {
	
	val layerModel=new LayerTableModel()
  val scaleModel=new ScaleModel
	
	var selectedInstances:Seq[InstanceData]=Seq.empty
	
	TestGraphListModel.controller=this
	
	val canvas=new GraphViewCanvas(this)
	val scalePanel=new ScalePanel(scaleModel,this)
	
	val canvasPanel=new BorderPanel {
		add(canvas,BorderPanel.Position.Center)
		add(scalePanel,BorderPanel.Position.North)
	}
	
	canvas.peer.addHierarchyBoundsListener( new HierarchyBoundsListener(){			
			def ancestorMoved(e:HierarchyEvent ) ={}
			
			def ancestorResized(e:HierarchyEvent )= {
				 //if(e.getChanged==canvas.peer)
					scaleModel.viewSize=canvas.size			
			}
	})
	
	val layerTable=new Table() {
		peer.setModel(layerModel)
		autoResizeMode=Table.AutoResizeMode.SubsequentColumns
		selection.intervalMode=Table.IntervalMode.Single
		selection.elementMode=Table.ElementMode.Row
	}
	
	val layerPanel = new BorderPanel() {
		add(new ScrollPane() {
			viewportView= layerTable
			preferredSize=new Dimension(100,100)
		},BorderPanel.Position.Center)
		add(new GridPanel(1,5){
			val addBut =new Button("Hinzufügen")
			val removeBut =new Button("Entfernen")
			val activateBut =new Button("Aktivieren")
			val visibleBut =new Button("Sichtbar")
			val edibleBut =new Button("Veränderbar")
			contents+=addBut+=removeBut+=visibleBut+=edibleBut+=activateBut
			listenTo(addBut,removeBut,visibleBut,edibleBut,activateBut)
			reactions+= {
				case ButtonClicked(`addBut`) => addLayer
				case ButtonClicked(`removeBut`) => removeLayer
				case ButtonClicked(`visibleBut`) => toggleVisible
			}
		},BorderPanel.Position.South)
	}
	
	def layerChanged(lay:Layer) = {
		zoomAll
	}
	
	def graphElemAdded(lay:Layer,elem:GraphElem) = {
		canvas.repaint
	}
	
	def graphElemRemoved(lay:Layer,elem:GraphElem) = {
		canvas.repaint
	}
	
	def graphElemChanged(lay:Layer,oldState:GraphElem,newState:GraphElem) = {
		canvas.repaint
	}
	// will be called when the DataViewController has another selection
	def selectionChanged(instList:Seq[InstanceData])= {
		selectedInstances=instList
	}
	
	def addLayer = {
		//println("sel:"+selectedInstances+" ref:"+selectedInstances.head.ref+" "+Layer.displayListTyp)
		if(selectedInstances!=null &&(!selectedInstances.isEmpty) && selectedInstances.head.ref.typ == Layer.displayListTyp 
				&& !layerModel.containsRef(selectedInstances.head.ref)) {
			
			val newLayer=Layer.createLayer(this,selectedInstances.head)
			layerModel.addLayer( newLayer)
			newLayer.load
			TestGraphListModel.update()
		}
	}
	
	def getSelectedLayer:Int = if ( layerTable.selection.rows.isEmpty) -1 else layerTable.selection.rows.head 
	
	def removeLayer = {
		 val ix=getSelectedLayer 
		 if(ix> -1) {
			 layerModel.layerList(ix).shutDown
			 layerModel.removeLayer(ix)
			 TestGraphListModel.update()
		 }
	}
	
	def toggleVisible = {
		val ix=getSelectedLayer 
		if(ix> -1)  layerModel.toggleVisibility(ix)	 
	}
	
	def zoomAll() = {
		val allBounds=layerModel.calcAllLayerBounds
		scaleModel.setWorldBounds(allBounds.x,allBounds.y,allBounds.width,allBounds.height)
		canvas.repaint
	}
	
	
}