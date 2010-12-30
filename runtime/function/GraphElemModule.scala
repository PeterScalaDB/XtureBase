/**
 * Author: Peter Started:04.10.2010
 */
package runtime.function

import server.storage.{ActionModule,ActionImpl,ActionIterator,CreateActionImpl}
import scala.collection.Iterator
import definition.expression.{Constant,VectorConstant}
import definition.data.{InstanceData,Reference,OwnerReference}
import definition.typ._
import transaction.handling.{TransactionManager,SessionManager}
import server.comm.UserSocket

/**
 * 
 */

trait GraphActionModule {
	def moveElement(elem:InstanceData,delta:VectorConstant):Unit
	def copyElement(elem:InstanceData,delta:VectorConstant):InstanceData
	
	var theTypeID:Int= -1
	
	def setObjectType(typeID:Int) = {
		//System.out.println("set object type ":+typeID+" "+this)
		theTypeID=typeID
		TypeInfos.moduleMap(theTypeID)=this
	}
}

class GraphElemModule extends ActionModule {
  var graphTypeID:Int= -1
  
	def actionList=List(moveAction,copyAction)	
  def getActionsIterator() = { actionList.iterator }
	def setObjectType(typeID:Int) =graphTypeID=typeID 
  
	val secondPointQuestion= Some(new DialogQuestion("nach Punkt",
		Seq(new ParamAnswerDefinition("Endpunkt Verschiebung",DataType.VectorTyp,None))))
		
	val dyQuestion= Some(new DialogQuestion("Eingabe Deltawert",
		Seq(new ParamAnswerDefinition("delta Y: ",DataType.DoubleTyp,None))))	
	
	val moveAction=new ActionIterator("Verschieben",Some(new DialogQuestion("Verschieben von Punkt / Delta",
		Seq(new ParamAnswerDefinition("Startpunkt Verschiebung",DataType.VectorTyp,secondPointQuestion),
			  new ParamAnswerDefinition("oder: Delta X:",DataType.DoubleTyp,dyQuestion)
			))),doMove)
	
	val copyAction=new ActionIterator("Kopieren",Some(new DialogQuestion("Kopieren von Punkt / Delta",
		Seq(new ParamAnswerDefinition("Startpunkt ",DataType.VectorTyp,secondPointQuestion),
			  new ParamAnswerDefinition("oder: Delta X:",DataType.DoubleTyp,dyQuestion)
			))),doCopy)	
  //val moveAction=new ActionImpl("Verschieben",None,nope)
  
  def nope(u:UserSocket, data:InstanceData,param:Seq[(String,Constant)]) =  {true}
	
	def doMove(u:UserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {		
		if(param.size==2) {
			val delta =
				if(param(0)._2.getType==DataType.VectorTyp )
				{
					val startPoint=param(0)._2.toVector
					val endPoint=param(1)._2.toVector					
					endPoint-startPoint
				}
				else if(param(0)._2.getType==DataType.DoubleTyp )
					new VectorConstant (param(0)._2.toDouble,param(1)._2.toDouble,0)
 			  else throw new IllegalArgumentException(" move wrong parametertype ")
			  //System.out.println("move delta:"+delta)
				for(d <-data) {
					TypeInfos.moduleMap(d.ref.typ).moveElement(d, delta)					
				}			  
		  true	
		}
		else false
	}
	
	def doCopy(u:UserSocket,owner:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {		
		if(param.size==2) {
			val delta =
				if(param(0)._2.getType==DataType.VectorTyp )
				{
					val startPoint=param(0)._2.toVector
					val endPoint=param(1)._2.toVector					
					endPoint-startPoint
				}
				else if(param(0)._2.getType==DataType.DoubleTyp )
					new VectorConstant (param(0)._2.toDouble,param(1)._2.toDouble,0)
 			  else throw new IllegalArgumentException(" move wrong parametertype ")				
				for(d <-data) {
					val createInst=TransactionManager.tryCreateInstance(d.ref.typ,d.owners,false)
					var newInst=d.clone(createInst.ref,d.owners,Seq.empty)
					newInst=TypeInfos.moduleMap(d.ref.typ).copyElement(newInst, delta)					
					TransactionManager.tryWriteInstanceData(newInst)
				}			
		  true	
		}
		else false
	}	
}

class LineModule extends ActionModule with GraphActionModule {
	val createActionList=List(createLineAction)	
	
	override def getCreateActionsIterator() =  createActionList.iterator
	override def getActionsIterator() =  Seq.empty.iterator
	
	def moveElement(elem:InstanceData,delta:VectorConstant) = {
		TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
		TransactionManager.tryWriteInstanceField(elem.ref,4,elem.fieldValue(4).toVector+delta)
	}
	def copyElement(elem:InstanceData,delta:VectorConstant) = {
		elem.setField(3,elem.fieldValue(3).toVector+delta).setField(4,
		elem.fieldValue(4).toVector+delta)
	}
	
	def nextPointQuestion:DialogQuestion=new DialogQuestion("Linie bis Punkt",Seq(
  	new ParamAnswerDefinition("weiterer Punkt",DataType.VectorTyp,None,"LT_Line")),true)
  def lineQuestion=new DialogQuestion("Linie erzeugen",Seq(new ParamAnswerDefinition("StartPunkt",DataType.VectorTyp,
  	Some(nextPointQuestion),"Create")))
	
	def createLineAction=new CreateActionImpl("Linie",Some(lineQuestion),doCreateLine)
	
	def doCreateLine(u:UserSocket,parents:Seq[InstanceData],param:Seq[(String,Constant)],newTyp:Int):Boolean= {
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
  	System.out.println("create line "+param.mkString)
  	System.out.println("newTyp:"+newTyp+" theTyp:"+theTypeID)
  			for(i <- 0 until param.size-1) {
  				val inst=TransactionManager.tryCreateInstance(theTypeID,parentRef,false)  				
  				val sp=param(i)._2.toVector
  				TransactionManager.tryWriteInstanceField(inst.ref,3,sp)
  				val ep=param(i+1)._2.toVector
  				TransactionManager.tryWriteInstanceField(inst.ref,4,ep)
  			}		
		true
	}
}

class ArcModule extends ActionModule with GraphActionModule {
	val createActionList=List()	
	
	override def getCreateActionsIterator() =  createActionList.iterator
	override def getActionsIterator() =  Seq.empty.iterator
	
	def moveElement(elem:InstanceData,delta:VectorConstant) = {
		TransactionManager.tryWriteInstanceField(elem.ref,3,elem.fieldValue(3).toVector+delta)
	}
	def copyElement(elem:InstanceData,delta:VectorConstant) = {
		elem.setField(3,elem.fieldValue(3).toVector+delta)
	}	
	
}


package object TypeInfos {	
	val moduleMap= collection.mutable.HashMap[Int,GraphActionModule]()	
	
}