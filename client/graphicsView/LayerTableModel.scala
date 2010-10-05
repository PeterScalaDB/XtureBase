/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView


import javax.swing._
import javax.swing.table._
import javax.swing.event._

import definition.data._
import definition.typ._
/**
 * 
 */
class LayerTableModel extends AbstractTableModel {
  val layerList=collection.mutable.ArrayBuffer[Layer]()
  val listLock=new Object()
  val activeLayer:Int=0
  val javaTrue=new java.lang.Boolean(true)
  val javaFalse=new java.lang.Boolean(false)
  
  def addLayer(newLayer:Layer) = listLock.synchronized{
  	layerList +=newLayer
  	val newSize=layerList.size
  	fireTableRowsInserted(newSize,newSize)
  }
  
  def removeLayer(ix:Int) = listLock.synchronized{
  	layerList.remove(ix)
  	fireTableRowsDeleted(ix,ix)
  }
  
  def changeLayerState(pos:Int,newState:Layer) = listLock.synchronized{
  	layerList(pos)=newState
  	fireTableRowsUpdated(pos,pos)
  }
  
  
  
  def getRowCount= listLock.synchronized{
		 //println("get size "+(dataList.size+1)+ " " +Thread.currentThread)
		 layerList.size		
	}
  
  def toggleVisibility(ix:Int) = {
  	val layer=layerList(ix)
  	if(layer.visible) layer.hide
  	else layer.load	
  	
  	fireTableRowsUpdated(ix,ix)
  }
  
  def containsRef(ref:Reference) = layerList.exists(_.ref ==ref)

	def getColumnCount= 4
	
	def boolToJava(value:Boolean) = if(value)javaTrue else javaFalse

	def getValueAt(row:Int,col:Int):Object = listLock.synchronized{
		if( row<layerList.size) {
			val layer=layerList(row)
			col match {
				case 0 => layer.name
				case 1 => boolToJava(layer.visible)
				case 2 => boolToJava(layer.edible)
				case 3 => boolToJava(row==activeLayer)
			}
		}
		else null
	}
	
	override def getColumnName(col:Int) =  {
		col match {
			case 0 =>"name"
			case 1 =>"sichtbar"
			case 2 =>"änderbar"
			case 3 =>"aktiv"
		}
	}
	
	override def getColumnClass(col:Int) = col match {
		case 0 => classOf[String]
		case 1 => classOf[java.lang.Boolean]
		case 2 => classOf[java.lang.Boolean]
		case 3 => classOf[java.lang.Boolean]
	}
}