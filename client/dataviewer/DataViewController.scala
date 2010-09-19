/**
 * Author: Peter Started:17.09.2010
 */
package client.dataviewer

import client.model._
import definition.data._
import definition.typ._

import scala.swing._

/** controls the DataViewer
 *  manages the general loading of data
 * 
 */
class DataViewController  extends PathControllable  {
	private var loaded=false
	var parentRef:Reference= _
	var mainClass:ObjectClass = _
	
	val propertyModels =scala.collection.mutable.ArrayBuffer[PropertyModel]()
	var numUsedModels=0
	
	val panel=new BoxPanel(Orientation.Vertical)
	
	var selectedInstance: InstanceData= _
	
	/** is called by PathModel when another instance should be loaded
	 *  @param parentRef the new Instance to be loaded
	 */
	def openData(nparentRef:Reference) = {
	  if(loaded) shutDown()
	  selectedInstance=null
	  parentRef=nparentRef
	  mainClass=AllClasses.getClassByID(parentRef.typ)
	  for(i <- 0 until mainClass.getPropFieldCount) {
	  	val propFieldInfo=mainClass.propField(i)
	  	val mod=getPropModel
	  	mod.load(propFieldInfo.allowedClass,i.toByte,propFieldInfo.name)
	  	panel.contents+=mod.panel
	  }
	  updateHeight()
	  loaded =true
	}
	
	def updateHeight() = {
		javax.swing.SwingUtilities.invokeLater(new Runnable(){
			def run= {
			  panel.preferredSize=new Dimension(10,getHeight)		
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
	
	def selectionChanged(tabMod:TypeTableModel,proMod:PropertyModel,inst:InstanceData) = {
		println("sel: propfield:"+proMod.propertyField+" typ:"+tabMod.typ +" "+inst)
		selectedInstance=inst
	}
	

}