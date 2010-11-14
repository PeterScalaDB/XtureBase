/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import definition.data._
import definition.typ._
import definition.comm._
import client.comm._
import scala.swing._
import scala.swing.event._
import javax.swing.{SwingUtilities,JList}
import java.awt.Color

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
	var listLock=new Object
	var tableModMap=scala.collection.mutable.HashMap[Int,TypeTableModel]()
	val vGlue=new ClickComp(this)
	val titleLabel=new Label("Prop")
	titleLabel.font=mainController.smallFont
	val tableArea=new BoxPanel (scala.swing.Orientation.Vertical ) {		
		//opaque=true
		//background=Color.green
	}
	
	
	val panel=new BorderPanel {
		add(titleLabel,BorderPanel.Position.North)
		add(tableArea,BorderPanel.Position.Center)
	}
	var selectRef:Option[Reference]=None
	
	/** loads data into the propertyModel
	 * 
	 * @param nallowedClass what classes are allowed to be in this model
	 * @param fieldToLoad what property field to load
	 * @param fieldName name of the property field
	 * @param selRef reference of an instance that should optionally be selected
	 */
	def load(nallowedClass:Int,fieldToLoad:Byte,fieldName:String,selRef:Option[Reference]) = {
		if(loaded) shutDown()
		selectRef=selRef
		allowedClass=nallowedClass
		propertyField=fieldToLoad
		titleLabel.text=" ("+fieldToLoad+") "+fieldName+(if(allowedClass==0) "" else  " erlaubt:"+ 
		AllClasses.get.getClassByID(allowedClass).name)
		titleLabel.horizontalAlignment=Alignment.Left
		if(subscriptionID<0)
			subscriptionID=ClientQueryManager.createSubscription(mainController.ref,propertyField)(callBack) 
		else { // subscription already there
			ClientQueryManager.changeSubscription(subscriptionID,mainController.ref,propertyField)
		}		
		loaded=true
	}
	def getPropFieldDefinition= mainController.mainClass.propFields (propertyField)
	
	
	def callBack(notType:NotificationType.Value,data: IndexedSeq[InstanceData]) = 
		ClientQueryManager.runSw  { listLock.synchronized{		
		//println("Proberty modification :"+notType+ " "+(if(data.isEmpty)" [Empty] "else   data.first.ref)+", ... "+	
		//		 "subsID:"+subscriptionID+ " ** "+ Thread.currentThread.getName)
		//println()				
		notType match {
			case NotificationType.sendData => {
				println("send data "+data)
				val grouped=data.view.groupBy(_.ref.typ)
				for((i,data) <-grouped.iterator) {
					val mod=if(tableModMap.contains(i)) tableModMap(i) else createTableModel(i)
					mod.setDataList(data,selectRef)
				}					
			}
			case NotificationType.childAdded => {
				println("child added:"+data)
				val typ=data(0).ref.typ
				val mod = if(tableModMap.contains(typ)) tableModMap(typ)
					else createTableModel(typ)					
				mod.addInstance(data(0))
			}
			case NotificationType.FieldChanged => {
				println("field added:"+data)
				val typ=data(0).ref.typ
				tableModMap(typ).changeInstance(data(0))
			}

			case NotificationType.instanceRemoved => {
				val typ=data(0).ref.typ
				tableModMap(typ).removeInstance(data(0).ref)							
			}		
		}}
	} 
	
	def createTableModel(typ:Int) = {
		//print("create new model "+typ)
		val newMod= if(tableModMap.contains(typ)) tableModMap(typ)
		else {
			val anewMod=new TypeTableModel(typ,this)
			tableModMap(typ)=anewMod
			anewMod
		}
		//print(" "+tableArea.contents.size)
		if(tableArea.contents.isEmpty)
			tableArea.contents +=newMod.scroller
			else tableArea.contents(tableArea.contents.size-1)=newMod.scroller
		tableArea.contents+=vGlue
		
		mainController.updateHeight
		tableArea.revalidate
		newMod
	}
	
	def sendDataToModels(data:Array[InstanceData]) = {
		
	}
	
	def getOwnerRef = new OwnerReference(propertyField,mainController.ref)
	
	def focusGained = mainController.containerFocused(propertyField)
	
	def shutDown() = listLock.synchronized{
		ClientQueryManager.pauseSubscription(subscriptionID)
		tableArea.contents.clear
		tableModMap.clear
		loaded=false
	}
	
	def deselect(selectedType:Int) = listLock.synchronized {
		//println("des")
		for(m <-tableModMap.valuesIterator;if(m.typ!=selectedType)) m.deselect()
	}
	
	def getHeight= listLock.synchronized {
		tableModMap.values.foldRight(0){(n,result)=> result+n.scroller.preferredSize.height}
	}
	
	
	class ClickComp(propMod:PropertyModel) extends Component {
		//val prefSiz=new Dimension(50,2000)		
		opaque=true
		background=Color.green
		focusable=true	
		peer.setTransferHandler(new PropAreaTransferHandler(propMod))
		
		//peer.setDropMode(DropMode.ON_OR_INSERT_ROWS)
		listenTo(mouse.clicks)
		reactions+= {			
			case e:MouseReleased => {				
				println("mouseclick "+peer.isFocusOwner+" "+size)
				requestFocus
				focusGained
			}
		//preferredSize=prefSiz	
	}
}
}

