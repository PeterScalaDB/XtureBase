/**
 * Author: Peter Started:09.10.2010
 */
package client.graphicsView

import client.dialog._
import scala.swing._
import scala.swing.event._
import definition.data._
import javax.swing.ImageIcon
import java.net.URL

import definition.typ.SelectGroup
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
		rowHeight= LayerPanelController.lineHeight
		focusable=false
		val colMod=peer.getColumnModel()		
		colMod.getColumn(0).setPreferredWidth(40)
		colMod.getColumn(1).setPreferredWidth(40)
		colMod.getColumn(2).setPreferredWidth(40)
		colMod.getColumn(3).setPreferredWidth(200)
		colMod.getColumn(4).setPreferredWidth(50)
		//colMod.getColumn(5).setPreferredWidth(70)
		//colMod.getColumn(6).setPreferredWidth(70)
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
  	def boolIdentity(o:Boolean)= o
  	
  	val eyeRend = new MyRenderer[Boolean] ( boolIdentity  ,LayerPanelController.eyeIcon,true)
  	val editRend = new MyRenderer[Boolean] ( boolIdentity  ,LayerPanelController.editIcon,true)
  	val newElemRend = new MyRenderer[Boolean] ( boolIdentity  ,LayerPanelController.newElemIcon,false)
  	override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
  		//FIND VALUE
  		val v = model.getValueAt(
  			peer.convertRowIndexToModel(row), 
  			peer.convertColumnIndexToModel(col))
  		//if(col<2) System.out.println("rendererComp col:"+col+" row:"+row+" v:"+v)
  		
  		
  			if (row >= viewController.layerModel .layerList .size) super.rendererComponent(sel,foc,row,col) 
  			else {  				
  				col match {
  					case 0 => eyeRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
  					case 1 => editRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
  					case 2 => newElemRend.componentFor(this, sel, foc, v.asInstanceOf[Boolean], row, col)
  					case _ => super.rendererComponent(sel,foc,row,col)
  				}  

  			}


  	}
  	
  	class MyRenderer[A](convert: A => Boolean,myIcon:ImageIcon,showIconUnselected:Boolean) extends Table.AbstractRenderer[A, Label](new Label) {	    
	   
	    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int) {
	      val checked = convert(a)	      
	      component.icon = if(showIconUnselected) myIcon else  {if (checked) myIcon else null } 
	      component.text = ""
	      component.background = if (checked) LayerPanelController.selectColor
	      else LayerPanelController.notSelectColor
	      //else component.background = table.background
	    }
	  }

	}
  
  def tableCellClicked(col:Int,row:Int)= {
  	if(col==3) {
  		if (row==viewController.layerModel.layerList.size) addLayer
  	}
  	else if(row<viewController.layerModel.layerList.size)
  	col match {  		 
  		case 0 => toggleVisible(row)
  		case 1 => toggleEdible(row)
  		case 2 => toggleActive(row)
  		case 4 => removeLayer(row) 
  		case _ =>
  	}
  }
  
  // instances from other models
  var selectedInstances:Seq[SelectGroup[_<: Referencable]]=Seq.empty
  
 /*val addBut =new Button("Hinzufügen")
	val removeBut =new Button("Entfernen")
	val activateBut =new Button("Aktivieren")
	val visibleBut =new Button("Sichtbar")
	val edibleBut:Button =new Button("Veränderbar")*/
  
  /*val layerPanel:BorderPanel = new BorderPanel() {
  	
		add(new ScrollPane() {
			viewportView= layerTable
			preferredSize=new Dimension(100,100)
		},BorderPanel.Position.Center)
		
	}*/
  
  def selectionChanged [T <: Referencable](sender:SelectSender,groups:Seq[SelectGroup[T]])= {
		selectedInstances=groups
		viewController.layerModel.setCanLoadElements(canLoad)		
	}
  
  def canLoad=  if (selectedInstances!=null &&(!selectedInstances.isEmpty)){
  	val firstList=selectedInstances.first.children
  	(!firstList.isEmpty) && firstList.head.ref.typ == Layer.displayListTyp &&
  	firstList.head.isInstanceOf[InstanceData]&& !viewController.layerModel.containsRef(firstList.head.ref)
  } else false
				 
  
  def addLayer = {
		//System.out.println("sel:"+selectedInstances+" ref:"+selectedInstances.head.ref+" "+Layer.displayListTyp)
		if(canLoad) {	
			for(group <-selectedInstances;aLayer <-group.children;if (aLayer.ref.typ == Layer.displayListTyp && 
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
  	viewController.canvas.repaint()
  }
  	 
  
  def getSelectedLayer:Int = if ( layerTable.selection.rows.isEmpty) -1 else layerTable.selection.rows.head 
	
	def removeLayer(ix:Int) = {		 
		 if(ix> -1) {
			 viewController.layerModel.layerList(ix).shutDown
			 viewController.layerModel.removeLayer(ix)			 
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

object LayerPanelController {
	val lineHeight=20
	val eyeIcon=createImageIcon("eye.gif")
  val editIcon=createImageIcon("editsmall.gif")
  val newElemIcon=createImageIcon("newElem.gif")
  val selectColor=new Color(70,160,230)
	val notSelectColor=new Color(250,250,250)
  
  def  createImageIcon(path:String):ImageIcon = {
		val imgURL:URL   = this.getClass.getResource(path)
		if (imgURL != null) {
			null
			return new ImageIcon(imgURL);
		} else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}
}
