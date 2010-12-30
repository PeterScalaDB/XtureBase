/**
 * Author: Peter Started:04.10.2010
 */
package client.graphicsView

import javax.swing._
import client.graphicsView._
import definition.data._
import client.comm._
import definition.comm._

/**
 * 
 */
object TestGraphListModel extends AbstractListModel {
	var controller:GraphViewController=_
	def dataList=controller.layerModel.layerList
	
	var subsID:Int=_
	
	def getSize(): Int = {
		if(controller==null) 0
  	val nsize=dataList.foldLeft(0)((result,elem)=>result+elem.elemList.size)
  	//System.out.println("TestMod size:"+nsize+ " datalist"+dataList.size)
  	nsize
 }

  def getElementAt(index: Int): Object = {
  	if(controller==null) null
  	findElem(0,index).toString
 	}
  
  def findElem(layerNr:Int,index:Int):GraphElem = {
  	if(dataList(layerNr).elemList.size>index) dataList(layerNr).elemList(index)
  	else findElem(layerNr+1,index-dataList(layerNr).elemList.size)
  }
  
  def update() = fireContentsChanged(this,0,getSize())
}