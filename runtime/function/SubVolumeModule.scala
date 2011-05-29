/**
 * Author: Peter Started:28.05.2011
 */
package runtime.function

import server.storage.ActionModule
import scala.collection.Iterator
import definition.typ._
import definition.data._
import server.storage.CreateActionImpl
import definition.expression.Constant
import server.comm.UserSocket
import transaction.handling.TransactionManager
import definition.expression.VectorConstant
import transaction.handling.SessionManager
import server.storage.CreateActionImpl
import transaction.handling.ActionList
import server.storage.StorageManager
import client.dataviewer.view3D.DataPrism
import client.dataviewer.view3D.PlaneElement
import client.dataviewer.view3D.PlaneFactory

/**
 * 
 */
class SubVolumeModule extends ActionModule {

  def getActionsIterator() = Nil.iterator
  
  override def getCreateActionsIterator=createActionList.iterator

  var volumeType:Int=0
  var planeType:Int=300 // warning: stored fixed, because plane type can not be found at time of module loading
  
  def setObjectType(typeID: Int): Unit = { volumeType=typeID }
  
  SessionManager.registerSetupListener(()=>{
  	planeType=AllClasses.get.getClassIDByName("Plane")
  })
  
  def createActionList=List(divideVolume)

  
  
  def heightQuestion=new DialogQuestion("Volumen Trennen durch welche Ebene ?",Seq(
  	new ParamAnswerDefinition("Trennebene auswählen",DataType.ObjectRefTyp,None,planeType.toString)))  
  
  val divideVolume=new CreateActionImpl("Teilvolumen",Some(heightQuestion),doDivideVolume)
  
  def doDivideVolume (u:UserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int):Boolean= {
  	if (parents.head.ref.typ==volumeType) { 
  		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))// field 0 for adding volumes
  		for(props <-ActionList.getInstanceProperties(parents.head.ref)){
  			if(!props.propertyFields(0).propertyList.isEmpty) 				
  				throw new IllegalArgumentException("Divide Volume impossible: has already child volumes !")  			
  			if(props.propertyFields(1).propertyList.size<5)
  				throw new IllegalArgumentException("Divide Volume impossible: Volume has to few border planes !")
  			val myPrism=PrismTools.loadPrism(parents.head.ref,props.propertyFields(1).propertyList)
  			
  			val thePlane=PlaneFactory(StorageManager.getInstanceData(param(0)._2 .toObjectReference))
  			
  			val topHitPoints=myPrism.getTopHitPointsWithIndex(thePlane)
  			val bottomHitPoints=myPrism.getBottomHitPointsWithIndex(thePlane)
  			val vertHitPoints=myPrism.getVerticalHitPoints(thePlane)
  			
  			if(!topHitPoints.isEmpty) {
  				if(!vertHitPoints.isEmpty) throw new IllegalArgumentException("Divide Volume impossible: top and vert hit points")
  				if(bottomHitPoints.isEmpty) throw new IllegalArgumentException("Divide Volume impossible: top but not bottom hit points")
  				if(topHitPoints.size!=2 || bottomHitPoints.size!=2)
  					throw new IllegalArgumentException("Divide Volume impossible: no 2 hitpoints in top or bottom")
  				for(i<-topHitPoints.indices)
  					if(topHitPoints(i)._2 !=bottomHitPoints(i)._2) 
  						throw new IllegalArgumentException("Divide Volume impossible: different hit edges in top and bottom plane")
  				
  				// divide vertically
  			} else if(!vertHitPoints.isEmpty) {
  				if(!bottomHitPoints.isEmpty) throw new IllegalArgumentException("Divide Volume impossible: vert and bottom hit points")
  				if(vertHitPoints.size<3) throw new IllegalArgumentException("Divide Volume impossible: to few vert hit points")
  				// divide horizontally
  				val fromOwner=new OwnerReference(1.toByte,parents.head.ref)
  				val topInst=TransactionManager.tryCreateInstance(volumeType,parentRef,false)  				
  				val topToOwner=new OwnerReference(1.toByte,topInst.ref)
  				TransactionManager.trySecondUseInstances(List(myPrism.planeList(0).ref),fromOwner,topToOwner,-1,false)
  				TransactionManager.trySecondUseInstances(List(thePlane.ref),thePlane.owners(0),topToOwner,-1,false)
  				TransactionManager.trySecondUseInstances((myPrism.planeList.drop(2)) map(_.ref),fromOwner,topToOwner,-1,false)
  				
  				val bottomInst=TransactionManager.tryCreateInstance(volumeType,parentRef,false)  				
  				val bottomToOwner=new OwnerReference(1.toByte,bottomInst.ref)
  				TransactionManager.trySecondUseInstances(List(thePlane.ref),thePlane.owners(0),bottomToOwner,-1,false)  				
  				TransactionManager.trySecondUseInstances((myPrism.planeList.drop(1)) map(_.ref),fromOwner,bottomToOwner,-1,false)
  			}  			
  			true
  		}
  	}				
  	false
  }
}


object PrismTools {
	
	def loadPrism(ref:Reference,planeInsts:Seq[Reference])= {
		new DataPrism(ref,(planeInsts map (r=>PlaneFactory(StorageManager.getInstanceData(r)))).toSeq)
	}
}