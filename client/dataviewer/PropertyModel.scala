/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import definition.data._
import definition.typ._
import definition.comm._
import client.comm._
import scala.swing._

/** manages all data changes of a property field of a instance
 * the PropertyModels are specialized on a certain allowed class
 * 
 * @param mainController the main controller managing the whole dataView
 * @param allowedClass what classes are allowed in this property field
 */
class PropertyModel(val mainController:DataViewController) {
	var propertyField:Byte=_
	var loaded=false
	var subscriptionID= -1
	var allowedClass:Int= _	
	var tableModMap=scala.collection.mutable.HashMap[Int,TypeTableModel]()
	
	val titleLabel=new Label("Prop")
	val tableArea=new BoxPanel (scala.swing.Orientation.Vertical )
	val panel=new BorderPanel {
		add(titleLabel,BorderPanel.Position.North)
		add(tableArea,BorderPanel.Position.Center)
	}
	
	
	def load(nallowedClass:Int,fieldToLoad:Byte,fieldName:String) = {
		if(loaded) shutDown()
		allowedClass=nallowedClass
		propertyField=fieldToLoad
		titleLabel.text="Property ("+fieldToLoad+") "+fieldName+ " allowedClass:"+allowedClass
		if(subscriptionID<0)
			subscriptionID=ClientQueryManager.createSubscription(mainController.parentRef,propertyField)(callBack) 
		else { // subscription already there
			ClientQueryManager.changeSubscription(subscriptionID,mainController.parentRef,propertyField)
		}		
		loaded=true
	}
	
	
	def callBack(notType:NotificationType.Value,data: IndexedSeq[InstanceData]) = {
				
		println("Property modification :"+notType+ " "+(if(data.isEmpty)" [Empty] "else   data.first.ref)+", ... "+	" subsID:"+subscriptionID)
		//println()				
		notType match {
			case NotificationType.sendData => {
				val grouped=data.view.groupBy(_.ref.typ)
				for((i,data) <-grouped.iterator) {
					val mod=createTableModel(i)
					mod.setDataList(data)
				}					
			}
			case NotificationType.childAdded => {
				val typ=data(0).ref.typ
				val mod = if(tableModMap.contains(typ)) tableModMap(typ)
					else createTableModel(typ)
				mod.addInstance(data(0))
			}
			case NotificationType.FieldChanged => {
				val typ=data(0).ref.typ
				tableModMap(typ).changeInstance(data(0))
			}

			case NotificationType.instanceRemoved => {
				val typ=data(0).ref.typ
				tableModMap(typ).removeInstance(data(0).ref)							
			}		
		}
	}
	
	def createTableModel(typ:Int) = {
		val newMod= if(tableModMap.contains(typ)) tableModMap(typ)
		else {
			val anewMod=new TypeTableModel(typ,this)
			tableModMap(typ)=anewMod
			anewMod
		}
		tableArea.contents +=newMod.scroller		
		//tableArea.revalidate
		mainController.updateHeight
		newMod
	}
	
	def sendDataToModels(data:Array[InstanceData]) = {
		
	}
	
	def getOwnerRef = new OwnerReference(propertyField,mainController.parentRef)
	
	
	def shutDown() = {
		ClientQueryManager.pauseSubscription(subscriptionID)
		tableArea.contents.clear
		tableModMap.clear
		loaded=false
	}
	
	def getHeight=tableModMap.size*100+40

}