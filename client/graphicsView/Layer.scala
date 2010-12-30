/**
 * Author: Peter Started:05.10.2010
 */
package client.graphicsView

import definition.data.{InstanceData,Reference,Referencable,OwnerReference}
import client.comm.ClientQueryManager
import definition.typ.AllClasses
import definition.comm.NotificationType
import java.awt.geom.Rectangle2D
import definition.expression.VectorConstant

/**
 * 
 */
class Layer(val controller:GraphViewController,override val ref:Reference,val name:String,var visible:Boolean,var edible:Boolean)
  extends Referencable {
	var subsID:Int= -1
	var elemList:Seq[GraphElem]=IndexedSeq.empty
  val bounds=new Rectangle2D.Double(0,0,0,0)
	var startTime:Long=_
	var firstLoad=true
	val ownerRef=new OwnerReference(0,ref)
	
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
							System.out.println("Layer "+name+" loadTime:"+(endTime-startTime)+" num elements:"+elemList.size+
								(if(!elemList.isEmpty)(" last Ref:"+elemList(elemList.size-1).ref.sToString) else ""))							 
							/*if(firstLoad){
								firstLoad=false*/
								calcBounds
								controller.layerChanged(this)
							//}							
							controller.graphElementsChanged(this,elemList,false)
							controller.canvas.repaint
						}
						
						case NotificationType.FieldChanged  => {
							val searchRef=data(0).ref							
								for(i <- elemList.indices)								
									if(searchRef ==elemList(i).ref){
										val oldState=elemList(i)
										elemList=elemList updated(i,data(0))
										//checkElemBounds(data(0))
										controller.graphElemChanged(this,data(0))
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
						case a => System.out.println("unhandled notification type "+a)
				}
			}
		}
	}
		
	def hide() = {
		visible=false
		ClientQueryManager.pauseSubscription(subsID)
		controller.selectModel.deselectLayer(this)
		elemList=IndexedSeq.empty
		controller.layerChanged(this)		
	}
	
	def lock() = {
		edible=false
		controller.selectModel.deselectLayer(this)		
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
		for(elem<-elemList) 
			checkElemBounds(elem)
		bounds.width-=bounds.x
		bounds.height-=bounds.y
		//System.out.println
		//System.out.println("layerbounds "+bounds)
		bounds
	}
	
	def filterSelection(filterFunc:(GraphElem)=>Boolean):Seq[GraphElem] = {
		if (edible) elemList.filter(filterFunc)
		else Seq.empty
	}
	
	def checkElementPoints(checkFunc:(GraphElem)=>Seq[(Byte,VectorConstant)]):Seq[(Byte,VectorConstant)]= {
		if (!visible) Seq.empty
		else {
			val buffer=elemList.flatMap(checkFunc)/*new collection.mutable.ArrayBuffer[(Byte,VectorConstant)]()
			for(el<-elemList) {
				val res=checkFunc(el)
				if(!res.isEmpty) 
					buffer ++=res
			}*/
				
			buffer
		}		
	}
	
	def checkElemBounds(elem:GraphElem) = {
		val eb=elem.getBounds
	  if (eb.x<bounds.x)bounds.x=eb.x
		if (eb.y<bounds.y)bounds.y=eb.y
		// use the width fields as maxX and height as maxY
		if (eb.width>(bounds.width))bounds.width=eb.width
		//print (" e.maxY:"+elem.maxY+" b.y:"+bounds.y+" b.h:"+bounds.height)
		if (eb.height>(bounds.height))bounds.height=eb.height	
	}
}




object Layer {
	var displayListTyp:Int=_
	ClientQueryManager.registerSetupListener(()=>{
		displayListTyp=AllClasses.get.getClassIDByName("DisplayList")
	})
	
	def createLayer(controller:GraphViewController,data:InstanceData) = {
		if(data.ref.typ!=displayListTyp) throw new IllegalArgumentException("Create Layer not possible for typ "+data.ref)
		new Layer(controller,data.ref,data.fieldValue(1).toString,true,true)
	}
}