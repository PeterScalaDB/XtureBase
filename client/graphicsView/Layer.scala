/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import definition.data.{InstanceData,Reference}
import client.comm.ClientQueryManager
import definition.typ.AllClasses
import definition.comm.NotificationType

/**
 * 
 */
class Layer(val controller:GraphViewController,val ref:Reference,val name:String,var visible:Boolean,var edible:Boolean) {
	var subsID:Int= -1
	var elemList:IndexedSeq[GraphElem]=IndexedSeq.empty
  
	
	
	
	def load() = {
		visible=true
		if(subsID>=0) ClientQueryManager.changeSubscription(subsID,ref,0)
		else subsID=ClientQueryManager.createFactSubscription(ref,0,GraphElemFactory){
			(ntype:NotificationType.Value,data:IndexedSeq[GraphElem]) => 
			ClientQueryManager.runSw{				
				ntype match {
						case NotificationType.sendData  => elemList=data ;controller.layerChanged(this)
						
						case NotificationType.FieldChanged  => {
							val searchRef=data(0).ref							
								for(i <- elemList.indices)								
									if(searchRef ==elemList(i).ref){
										val oldState=elemList(i)
										elemList=elemList updated(i,data(0))
										controller.graphElemChanged(this,oldState,data(0))
									}
						}
						case NotificationType.instanceRemoved => {
							val searchRef=data(0).ref
							val oldElem=elemList.find(searchRef==_.ref)							
							if(oldElem.isDefined) {
								elemList =elemList.filter(searchRef!= _.ref)
								controller.graphElemRemoved(this,oldElem.get)
							}								
						}
						case NotificationType.childAdded => {
							elemList= elemList :+ data(0)
							controller.graphElemAdded(this,data(0))
						}
						case a => println("unhandled notification type "+a)
				}
			}
		}
	}
		
	def hide() = {
		visible=false
		ClientQueryManager.pauseSubscription(subsID)
		elemList=IndexedSeq.empty
		controller.layerChanged(this)
	}
	
	def shutDown() = {
		ClientQueryManager.removeSubscription(subsID)
		elemList=IndexedSeq.empty
		controller.layerChanged(this)
	}	
	
}


object Layer {
	var displayListTyp:Int=_
	ClientQueryManager.registerSetupListener(()=>{
		displayListTyp=AllClasses.get.getClassIDByName("DisplayList")
	})
	
	def createLayer(controller:GraphViewController,data:InstanceData) = {
		if(data.ref.typ!=displayListTyp) throw new IllegalArgumentException("Create Layer not possible for typ "+data.ref)
		new Layer(controller,data.ref,data.fieldValue(0).toString,true,true)
	}
}