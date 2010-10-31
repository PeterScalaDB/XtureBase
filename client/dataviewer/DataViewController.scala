/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import client.model._
import definition.data._
import definition.typ._
import client.dialog._

import scala.swing._
import java.awt.Color

/** controls the DataViewer
 *  manages the general loading of data
 * 
 */


class DataViewController  extends PathControllable with SelectSender with Referencable  {
	private var loaded=false
	var ref:Reference= _
	var mainClass:AbstractObjectClass = _
	
	val propertyModels =scala.collection.mutable.ArrayBuffer[PropertyModel]()
	var numUsedModels=0
	
	val panel=new BoxPanel(Orientation.Vertical) 
	
	var selectedInstance: InstanceData= _ // to be deleted
	
	var openChildCallBack:(Reference)=> Unit = _
	var selectListener= collection.mutable.HashSet[SelectListener]()
	
	var containerFocusListener= collection.mutable.HashSet[ContainerFocusListener]()
	
	/** is called by PathModel when another instance should be loaded
	 *  @param parentRef the new Instance to be loaded
	 *  @param selectRef reference of an instance that should be selected
	 */
	def openData(nparentRef:Reference,selectRef:Option[Reference]) = {
	  if(loaded) shutDown()
	  selectedInstance=null
	  ref=nparentRef
	  mainClass=AllClasses.get.getClassByID(ref.typ)
	  for(i <- 0 until mainClass.propFields.size) {
	  	val propFieldInfo=mainClass.propFields(i)
	  	val mod=getPropModel
	  	mod.load(propFieldInfo.allowedClass,i.toByte,propFieldInfo.name,selectRef)
	  	panel.contents+=mod.panel
	  	
	  }
	  updateHeight()
	  if(!selectRef.isDefined) selectListener foreach(_.selectionChanged(this,null))
	  loaded =true
	}
	
	def registerOpenChildCallBack(callBack: (Reference)=> Unit) = {
		openChildCallBack=callBack
	}
	
	def registerSelectListener(listener:SelectListener) = {
		selectListener += listener
	}
	
	def registerContainerFocusListener(listener:ContainerFocusListener) = {
		containerFocusListener += listener
	} 
	
	
	def updateHeight() = {
		javax.swing.SwingUtilities.invokeLater(new Runnable(){
			def run= {
			  //panel.preferredSize=new Dimension(10,getHeight)		
			  panel.revalidate
			  panel.repaint	
			}
		})	
	}
	
	def getHeight=propertyModels.take(numUsedModels).foldRight(0){(n,result)=> result+n.getHeight}
	
	
	def getPropModel = {
		numUsedModels  += 1
		if(propertyModels.size>=numUsedModels) propertyModels(numUsedModels-1)		
		else {
			val newMod= new PropertyModel(this)
			propertyModels append newMod
			newMod
		}
	}
	
	
	def shutDown() = {
		panel.contents.clear
		updateHeight()
		for(i <-0 until numUsedModels) propertyModels(i).shutDown()
		// save the models for later use		
		numUsedModels=0		
		loaded=false		
	}
	
	def selectionChanged(tabMod:TypeTableModel,proMod:PropertyModel,instList:Seq[InstanceData]):Unit = {
		//println("sel: propfield:"+proMod.propertyField+" typ:"+tabMod.typ +" \n"+instList.mkString)
		for(i <- 0 until numUsedModels;val mod=propertyModels(i))
			mod.deselect(tabMod.typ)
		 selectListener foreach(_.selectionChanged(this,instList))	
		//selectedInstance=inst
	}
	
	def containerFocused(currPropertyField:Int):Unit = {
		containerFocusListener foreach (_.containerFocused(this,currPropertyField,""))
	}
	
	def deselect(notify:Boolean) = {
		for(i <- 0 until numUsedModels;val mod=propertyModels(i))
			mod.deselect(-1)
		
	}
	
	/** sends a message to the path controller that it should open a child instance
	 * 
	 * @param ref the reference of the child instance
	 */
	def openChild(ref:Reference) = {
		if(openChildCallBack!=null) openChildCallBack(ref)
	}
	

}