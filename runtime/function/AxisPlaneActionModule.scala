/**
 * Author: Peter Started:27.05.2011
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

/**
 * 
 */
class AxisPlaneActionModule extends ActionModule {

  def getActionsIterator() = Nil.iterator
  
  override def getCreateActionsIterator:Iterator[AbstractAction]=createActionList.iterator
  
  def createActionList=List(createHorPlane,createVertPlane)
  
  def setObjectType(typeID: Int): Unit = { 
  	planeTypeID=typeID 	
  	}
  
  SessionManager.registerSetupListener(()=>{
  	subVolumeTypeID=AllClasses.get.getClassIDByName("Teilvolumen")
  })
  
  var planeTypeID:Int= -1
  var subVolumeTypeID:Int = -1
  
  def heightQuestion=new DialogQuestion("Horizontale Ebene erzeugen",Seq(new ParamAnswerDefinition("Höhe",DataType.DoubleTyp,
  	None)))  
  
  val createHorPlane=new CreateActionImpl("Hor. Ebene",Some(heightQuestion),doCreateHorPlane)
  
  def doCreateHorPlane (u:UserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int):Boolean= {
  	if (parents.head.ref.typ==subVolumeTypeID) { 
		val parentRef=Array(new OwnerReference(2.toByte,parents.head.ref))
  	System.out.println("create plane "+param.mkString)
  	System.out.println("newTyp:"+newTyp+" theTyp:"+planeTypeID)
  			
  	val inst=TransactionManager.tryCreateInstance(planeTypeID,parentRef,false)  				
  				val sp=new VectorConstant(0,0,param(0)._2.toDouble)
  				TransactionManager.tryWriteInstanceField(inst.ref,1,sp)  				
  				TransactionManager.tryWriteInstanceField(inst.ref,2,new VectorConstant(0,0,1))
  	 true			
  	}				
  	else false
	}
  
  def horDirQuestion=new DialogQuestion("Vertikale Ebene erzeugen",Seq(new ParamAnswerDefinition("1. Punkt",DataType.VectorTyp,
  	Some(new DialogQuestion("Vertikale Ebene 2.Punkt",Seq(new ParamAnswerDefinition("2. Punkt",DataType.VectorTyp,None))))
  	)))
  
  val createVertPlane=new CreateActionImpl("Vert. Ebene",Some(horDirQuestion),doCreateVertPlane)
  
  def doCreateVertPlane(u:UserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int):Boolean= {
  	if (parents.head.ref.typ==subVolumeTypeID) { 
		val parentRef=Array(new OwnerReference(2.toByte,parents.head.ref))
  	System.out.println("create plane "+param.mkString)
  	System.out.println("newTyp:"+newTyp+" theTyp:"+planeTypeID)  			
  	val inst=TransactionManager.tryCreateInstance(planeTypeID,parentRef,false) 				
  				
  	val dist=param(1)._2.toVector-param(0)._2.toVector
  	if(dist.x!=0 || dist.y!=0) {
  		TransactionManager.tryWriteInstanceField(inst.ref,1,param(0)._2.toVector)  					
  		val vert=new VectorConstant(0,0,1)
  		val norm=dist.cross(vert).unit
  		TransactionManager.tryWriteInstanceField(inst.ref,2,norm)
  	} else throw new IllegalArgumentException("cant create vertical plane, def points are vertically aligned")
  				
  	 true			
  	}				
  	else false
  }
}