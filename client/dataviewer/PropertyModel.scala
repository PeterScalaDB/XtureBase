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
import javax.swing.BorderFactory
import javax.swing.border._

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
	var isFirstPropField:Boolean=false
	var isLoading:Boolean=true // is the callBack routine called for the first time (true) or as a refresh after a move(false) 
	var tableModMap=scala.collection.mutable.HashMap[Int,TypeTableModel]()
	val vGlue=new ClickComp(this)
	val titleLabel=new Label("Prop")
	titleLabel.font=mainController.smallFont
	val tableArea=new BoxPanel (scala.swing.Orientation.Vertical ) {		
		contents+=vGlue
		
	}
	val vGlueMaxSize=new Dimension(0,30)
	val vGlueMinSize=new Dimension(0,30)
	
	val panel=new BorderPanel {
		add(titleLabel,BorderPanel.Position.North)
		add(tableArea,BorderPanel.Position.Center)
		border= BorderFactory.createCompoundBorder(
			BorderFactory.createEtchedBorder(EtchedBorder.LOWERED),
			BorderFactory.createLineBorder(background,3))
		//opaque=true
		//background=Color.blue
		//maximumSize=new Dimension(Short.MaxValue,Short.MaxValue)
	}
	var selectRef:Option[Reference]=None
	
	/** loads data into the propertyModel
	 * 
	 * @param nallowedClass what classes are allowed to be in this model
	 * @param fieldToLoad what property field to load
	 * @param fieldName name of the property field
	 * @param selRef reference of an instance that should optionally be selected
	 * @Param nfirstPropField is this the first property field
	 * @param onlyPropField is this the only property field
	 */
	def load(nallowedClass:Int,fieldToLoad:Byte,fieldName:String,selRef:Option[Reference],nfirstPropField:Boolean,onlyPropField:Boolean) = {
		if(loaded) shutDown()
		selectRef=selRef
		allowedClass=nallowedClass
		propertyField=fieldToLoad
		isFirstPropField=nfirstPropField
		titleLabel.text=" ("+fieldToLoad+") "+fieldName+(if(allowedClass==0) "" else  " erlaubt:"+ 
		AllClasses.get.getClassByID(allowedClass).name)
		titleLabel.visible= !onlyPropField
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
					//println("send data field:"+propertyField+" size:"+data.size)
					if(data.size==0) {
						if(isFirstPropField){
							vGlue.requestFocusInWindow						
							focusGained
						}
						vGlue.preferredSize=vGlueMaxSize
						vGlue.revalidate
					}
					else {
						vGlue.preferredSize=vGlueMinSize
						val grouped:Map[Int,Seq[InstanceData]]=data.view.groupBy(_.ref.typ)
						for((i,data) <-grouped.iterator) {
							val mod=if(tableModMap.contains(i)) tableModMap(i) else createTableModel(i)
							mod.setDataList(data,selectRef,!isLoading)
						}	
					}
				}
				case NotificationType.childAdded => {
					//println("child added:"+data)
					val typ=data(0).ref.typ
					val mod = if(tableModMap.contains(typ)) tableModMap(typ)
					else createTableModel(typ)					
					mod.addInstance(data(0))
				}
				case NotificationType.FieldChanged => {
					//println("field added:"+data)
					val typ=data(0).ref.typ
					if(tableModMap.contains(typ))
						tableModMap(typ).changeInstance(data(0))
				}

				case NotificationType.instanceRemoved => {
					val typ=data(0).ref.typ
					if(tableModMap.contains(typ))
						tableModMap(typ).removeInstance(data(0).ref)							
				}		
			}
			selectRef=None
			isLoading=false
		}
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
			else {				
				tableArea.contents(tableArea.contents.size-1)=Swing.VStrut(10)
				tableArea.contents+=	newMod.scroller
			}
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
		tableArea.contents+=vGlue
		tableModMap.clear
		loaded=false
	}
	
	
	
	def deselect(selectedType:Int) = listLock.synchronized {
		//println("des")
		for(m <-tableModMap.valuesIterator;if(m.typ!=selectedType)) m.deselect()
	}
	
	/*def getHeight= listLock.synchronized {
		val theHeight=tableModMap.values.foldRight(0){(n,result)=> result+n.scroller.preferredSize.height}
		if(theHeight<10) 30 else theHeight
	}
	
	def getWidth= listLock.synchronized {
		tableModMap.values.foldRight(0){(n,result)=> 
			if(n.scroller.preferredSize.width>result)n.scroller.preferredSize.width else result }
	}*/
	
	class ClickComp(propMod:PropertyModel) extends Component {
		val prefSiz=new Dimension(Short.MaxValue,Short.MaxValue)		
		opaque=true
		background=Color.yellow
		focusable=true	
		peer.setTransferHandler(new PropAreaTransferHandler(propMod))
		maximumSize=prefSiz
		
		
		//peer.setDropMode(DropMode.ON_OR_INSERT_ROWS)
		listenTo(mouse.clicks)
		reactions+= {			
			case e:MouseReleased => {				
				//println("mouseclick "+peer.isFocusOwner+" "+size)
				requestFocus
				focusGained
			}
		//preferredSize=prefSiz	
	}
}
}

