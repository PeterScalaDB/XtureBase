/**
 * Author: Peter Started:16.09.2010
 */
package client.model

import javax.swing.AbstractListModel

import definition.data._
import client.comm._
import definition.comm._
import scala.collection.immutable.IndexedSeq
import javax.swing.SwingUtilities

/**
 * 
 */
class PathModel extends AbstractListModel {
	
	var subsID= -1
	var dataList:Option[collection.immutable.Seq[InstanceData]]= None	
	
	private [model] def loadPath(newPath:Seq[Reference])(readyFunc:() => Unit) = {
		if(subsID== -1) {
			subsID= ClientQueryManager.createPathSubscription(newPath) { 
				(ntype: NotificationType.Value,data:IndexedSeq[InstanceData]) => {
					//System.out.println("Path notification:"+ntype+" subsID:"+subsID+" data:"+data.map(_.ref).mkString(","))
					val oldSize=dataList match { case Some(list) => list.size;case None => 0 }
					ntype match {
						case NotificationType.sendData  =>{
							System.out.println("path send data "+data.mkString)
							dataList=Some(data) 
						}
						case NotificationType.FieldChanged  => {
							System.out.println("path field changed "+data(0))
							val searchRef=data(0).ref
							for (list <- dataList)
								for(i <- list.indices)								
									if(searchRef ==list(i).ref) dataList=Some(list updated(i,data(0)))
						}
						case NotificationType.instanceRemoved => {
							val searchRef=data(0).ref							
							dataList =Some(dataList.get.filter(searchRef!= _.ref))
						}
						case a => System.out.println("unhandled notification type "+a)
					}
					//SwingUtilities.invokeLater(new Runnable {
					//def run () = {
					val newSize=dataList.get.size
					if (newSize>oldSize) fireIntervalAdded(this,oldSize,newSize-1)
					else if(newSize<oldSize) fireIntervalRemoved(this,newSize,oldSize-1)
					else  fireContentsChanged(this,0,newSize)
					//}})
					//System.out.println("Fire changed "+dataList.get.size)
					if(ntype==NotificationType.sendData) readyFunc()
				}
			}
		}
		else { // subscription exists already
			ClientQueryManager.pathSubs_changePath(subsID,newPath)
		}
	}
	
	private [model] def addPathElement(newElement:Reference) = 
		if(subsID> -1) {
			ClientQueryManager.pathSubs_addPathElement(subsID,newElement)			
		}
			
		
	
	
	private [model] def jumpUp(newPos:Int) = 
		if(subsID> -1) { 
			//System.out.println("Jumping to "+newPos)
			dataList=Some(dataList.get.take(newPos+1))
			ClientQueryManager.pathSubs_jumpUp(subsID,newPos)
			fireContentsChanged(this,0,dataList.get.size)
		}
	
		
		
  def getSize(): Int = { 
  	for(list <-dataList) return list.size
  	return 0
  }

  def getElementAt(index: Int): Object = 
  	if (dataList.isDefined) {  		
  		getInstanceAt(index)
		} else null
 	

  
  def getInstanceAt(index:Int  ):InstanceData = {
  	for(list <-dataList)
  		if(list.size>index) return list(index)
  	return null
  }
  
  def shutDown() = {
  	if(subsID > -1) {
  		ClientQueryManager.removeSubscription(subsID)
  		subsID= -1
  	}
  }
  
  def getTitleText:String = {
		if(dataList.isDefined) dataList.get.mkString(" \\ ")
		else " empty "
	}

}