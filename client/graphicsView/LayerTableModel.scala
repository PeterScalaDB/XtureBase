/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView


import javax.swing._
import javax.swing.table._
import javax.swing.event._
import java.awt.geom.Rectangle2D

import definition.data._
import definition.typ._
import definition.expression.VectorConstant
/**
 * 
 */
class LayerTableModel(controller:GraphViewController) extends AbstractTableModel {
  val layerList=collection.mutable.ArrayBuffer[Layer]()
  val listLock=new Object()
  var activeLayer:Int=0
  val javaTrue=new java.lang.Boolean(true)
  val javaFalse=new java.lang.Boolean(false)
  val newElemLayer=new NewElemLayer(controller)
  
  var sizeChangeListener:(Int) => Unit = null
  
  var canLoadElements:Boolean=false
  
  def addLayer(newLayer:Layer) = listLock.synchronized{
  	layerList +=newLayer
  	val newSize=layerList.size
  	fireTableRowsInserted(newSize,newSize)
  	notifySizeChanged()
  }
  
  def removeLayer(ix:Int) = listLock.synchronized{
  	layerList.remove(ix)
  	fireTableRowsDeleted(ix,ix)
  	notifySizeChanged()
  }
  
  def removeAllLayers() = listLock.synchronized {
  	layerList.foreach (_.shutDown)
  	layerList.clear
  	notifySizeChanged()
  }
  
  def changeLayerState(pos:Int,newState:Layer) = listLock.synchronized{
  	layerList(pos)=newState
  	fireTableRowsUpdated(pos,pos)  	
  }
  
  /** checks if there are visible Layers
   * 
   */
  def hasVisibleLayers= 
  	layerList.exists(_.visible)
  
  def setActiveLayerIx(layerNum:Int)= {
  	activeLayer=layerNum
  	controller.notifyContainerListeners
  	fireTableDataChanged()
  }
  
  def getActiveLayer = listLock.synchronized{
  	if(layerList.isEmpty) null
  	else layerList(activeLayer)
  }
  
  def getLabelText = {
  	val al=getActiveLayer
  	val activeName=if(al==null) "empty" else al.name
  	val edibleNames=layerList.filter(el => el.ref!=al.ref && el.edible).map(_.name).mkString(", ")
  	val visibleNames= layerList.filter(el => el.ref!=al.ref && !el.edible&&el.visible).map(_.name).mkString(", ")
  	activeName+" | "+edibleNames+" | "+visibleNames
  }
  
  def registerSizeChangeListener(newList:(Int) => Unit) =  {
  	sizeChangeListener=newList
  	notifySizeChanged
  }
  
  def notifySizeChanged() = if(sizeChangeListener!=null) sizeChangeListener(layerList.size)
  
  def toggleVisibility(ix:Int):Unit = listLock.synchronized{
  	val layer=layerList(ix)
  	if(layer.visible) {
  		layer.visible=false  	
  		layer.edible=false
  		if(activeLayer==ix) {
  			val nextActive=getNextActiveLayer
  			if(nextActive== -1) {
  				layer.visible=true
  				layer.edible=true
  				return // dont hide layer if it is the only edible
  			}
  			else activeLayer=nextActive
  		}
  		layer.hide  		
  	}
  	else layer.load	  	
  	fireTableDataChanged()
  	
  }
  
  def toggleEdible(ix:Int):Unit = listLock.synchronized{
  	val layer=layerList(ix)
  	if(layer.edible) {
  		layer.edible=false  		
  		if(activeLayer==ix) {
  			val nextActive=getNextActiveLayer
  			if(nextActive== -1) {
  				layer.edible=true
  				return // dont hide layer if it is the only edible
  			}
  			else activeLayer=nextActive
  		}
  		layer.lock()
  	}
  	else {layer.edible=true;if(!layer.visible)toggleVisibility(ix)}	  	
  	fireTableDataChanged()  	
  }
  
  def toggleActive(ix:Int):Unit = listLock.synchronized{
  	val layer=layerList(ix)
  	if(!layer.visible) layer.load
  	layer.edible=true
  	setActiveLayerIx(ix)
  }
  
  
  def getNextActiveLayer:Int= {
  		for(i <- 0 until layerList.size;val alay=layerList(i))
  			if(alay.visible && alay.edible) return i
  			-1
  	}
  
  
  def containsRef(ref:Reference) = layerList.exists(_.ref ==ref)

  def getRowCount= listLock.synchronized{		 
		 layerList.size	+1	
	}
  
	def getColumnCount= 5
	
	def boolToJava(value:Boolean) = if(value)javaTrue else javaFalse

	def getValueAt(row:Int,col:Int):Object = listLock.synchronized{
		if( row<layerList.size) {
			val layer=layerList(row)
			col match {				
				case 0 => layer.visible.asInstanceOf[AnyRef]
				case 1 => layer.edible.asInstanceOf[AnyRef]
				case 2 => (row==activeLayer).asInstanceOf[AnyRef]
				case 3 => layer.name
				case 4 => " X "
				case _ => ""
			}
		}
		else if(row==layerList.size&& col==3) // last line
			if(canLoadElements)"  --> Layer hinzufügen " else " - "
		else null
	}
	
	override def getColumnName(col:Int) =  {
		col match {
			case 0 =>"Sich"
			case 1 =>"Änd"
			case 2 =>"Neu"
			case 3 =>"Layer-Name"
			case 4 =>"Raus"
			//case 5 =>"El.Vers"
			//case 6 =>"El.Kop"
		}
	}
	
	override def getColumnClass(col:Int) = col match {		
		case 0 => classOf[Boolean]
		case 1 => classOf[Boolean]
		case 2 => classOf[java.lang.Boolean]
		case 3 => classOf[String]
		case 4 => classOf[javax.swing.JButton]
		//case 5 => classOf[String]
		//case 6 => classOf[java.lang.Boolean]
	}
	
	def calcAllLayerBounds()= {
		val bounds=new Rectangle2D.Double
		bounds.x=Math.MAX_DOUBLE
		bounds.y=Math.MAX_DOUBLE
		bounds.width=Math.MIN_DOUBLE
		bounds.height=Math.MIN_DOUBLE
		for(lay <-layerList; if (lay.visible)) 
			doLayerCheck(lay,bounds)
		doLayerCheck(newElemLayer,bounds)
		bounds.width-=bounds.x
		bounds.height-=bounds.y
		bounds
	}
	
	private def doLayerCheck(lay:Layer,bounds:Rectangle2D.Double) = {
		val lb=lay.calcBounds
		if(lb.x<bounds.x)bounds.x=lb.x
		if(lb.y<bounds.y)bounds.y=lb.y
		// use bounds.width as maxX
		if(lb.x+lb.width>bounds.width)bounds.width=lb.x+lb.width
		if(lb.y+lb.height>bounds.height)bounds.height=lb.y+lb.height
	}
	
	def setCanLoadElements(value:Boolean)= {
		canLoadElements=value
		fireTableDataChanged() 
	}
	
	def checkElementPoints(checkFunc:(GraphElem)=>Seq[(Byte,VectorConstant)]):Seq[(Byte,VectorConstant)]= {
		val ret1=layerList.flatMap(_.checkElementPoints(checkFunc))
		val ret2=(newElemLayer.checkElementPoints(checkFunc))
		collection.mutable.ArrayBuffer()++=ret1++=ret2		
	}
	
	def filterLayersSelection(filtFunc:(GraphElem)=>Boolean):Seq[GraphElem]= {
		val ret1=layerList.flatMap(_.filterSelection(filtFunc))
		val ret2=newElemLayer.filterSelection(filtFunc)
		collection.mutable.ArrayBuffer()++=ret1++=ret2
	}
}