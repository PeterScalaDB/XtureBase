/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import definition.data.{InstanceData,Reference}
import client.comm.ClientQueryManager
import definition.typ.AllClasses
import definition.comm.NotificationType
import java.awt.geom.Rectangle2D
import definition.expression.VectorConstant

/**
 * 
 */
class Layer(val controller:GraphViewController,val ref:Reference,val name:String,var visible:Boolean,var edible:Boolean) {
	var subsID:Int= -1
	var elemList:IndexedSeq[GraphElem]=IndexedSeq.empty
  val bounds=new Rectangle2D.Double(0,0,0,0)
	var startTime:Long=_
	
	
	def load() = {
		visible=true
		startTime=System.currentTimeMillis();
		if(subsID>=0) ClientQueryManager.changeSubscription(subsID,ref,0)
		else subsID=ClientQueryManager.createFactSubscription(ref,0,GraphElemFactory){
			(ntype:NotificationType.Value,data:IndexedSeq[GraphElem]) => 
			ClientQueryManager.runSw{				
				ntype match {
						case NotificationType.sendData  =>{
							val endTime=System.currentTimeMillis();
							elemList=data
							println("Layer "+name+" loadTime:"+(endTime-startTime)+" num elements:"+elemList.size+
								(if(!elemList.isEmpty)(" last Ref:"+elemList(elemList.size-1).ref.sToString) else ""))
							 
							calcBounds
							controller.layerChanged(this)
						}
						
						case NotificationType.FieldChanged  => {
							val searchRef=data(0).ref							
								for(i <- elemList.indices)								
									if(searchRef ==elemList(i).ref){
										val oldState=elemList(i)
										elemList=elemList updated(i,data(0))
										//checkElemBounds(data(0))
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
							//checkElemBounds(data(0))
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
	
	def getBounds=bounds
	
	def calcBounds()={
		bounds.x=Math.MAX_DOUBLE
		bounds.y=Math.MAX_DOUBLE
		bounds.width=Math.MIN_DOUBLE
		bounds.height=Math.MIN_DOUBLE
		for(elem<-elemList) checkElemBounds(elem)
		bounds.width-=bounds.x
		bounds.height-=bounds.y
		//println
		//println("layerbounds "+bounds)
		bounds
	}
	
	def filterSelection(filterFunc:(GraphElem)=>Boolean):Seq[GraphElem] = {
		if (edible) elemList.filter(filterFunc)
		else Seq.empty
	}
	
	def checkElementPoints(checkFunc:(GraphElem)=>Option[VectorConstant]):Seq[VectorConstant]= {
		if (!visible) Seq.empty
		else {
			val buffer=new collection.mutable.ArrayBuffer[VectorConstant]()
			for(el<-elemList) {
				val res=checkFunc(el)
				if(res.isDefined) 
					buffer +=res.get
			}
			buffer
		}		
	}
	
	def checkElemBounds(elem:GraphElem) = {
	  if (elem.minX<bounds.x)bounds.x=elem.minX
		if (elem.minY<bounds.y)bounds.y=elem.minY
		// use the width fields as maxX and height as maxY
		if (elem.maxX>(bounds.width))bounds.width=elem.maxX
		//print (" e.maxY:"+elem.maxY+" b.y:"+bounds.y+" b.h:"+bounds.height)
		if (elem.maxY>(bounds.height))bounds.height=elem.maxY	
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