/**
 * Author: Peter Started:04.10.2010
 */
package client.model

import javax.swing._
import client.graphicsView._
import definition.data._
import client.comm._
import definition.comm._

/**
 * 
 */
object TestGraphListModel extends AbstractListModel {
	var dataList:IndexedSeq[GraphElem]=IndexedSeq.empty
	var subsID:Int=_
	
	def getSize(): Int = { 
  	dataList.size  	
 }

  def getElementAt(index: Int): Object = {
  	dataList(index)
 	}
  
  def load(parentRef:Reference,propField:Byte)= {
		subsID=ClientQueryManager.createFactSubscription(parentRef,propField,GraphElemFactory){
			(ntype:NotificationType.Value,data:IndexedSeq[GraphElem]) => {
				val oldSize=dataList.size
				ntype match {
						case NotificationType.sendData  => dataList=data ;fireContentsChanged(this,0,dataList.size)
						case NotificationType.FieldChanged  => {
							val searchRef=data(0).ref							
								for(i <- dataList.indices)								
									if(searchRef ==dataList(i).ref){
										dataList=dataList updated(i,data(0))
										fireContentsChanged(this,i,i)
									}
						}
						case NotificationType.instanceRemoved => {
							val searchRef=data(0).ref							
							dataList =dataList.filter(searchRef!= _.ref)
							val newSize=dataList.size
							fireIntervalRemoved(this,newSize,newSize)
						}
						case NotificationType.childAdded => {
							dataList= dataList :+ data(0)
							fireIntervalAdded(this,dataList.size-1,dataList.size-1)
						}
						case a => println("unhandled notification type "+a)
					}
				
				
			}
		}
  }
}