/**
 * Author: Peter Started:04.10.2010
 */
package runtime.function

import server.storage.{ActionModule,ActionImpl,ActionIterator}
import scala.collection.Iterator
import definition.expression.{Constant,VectorConstant}
import definition.data.{InstanceData,Reference,OwnerReference}
import definition.typ._
import transaction.handling.{TransactionManager,SessionManager}

/**
 * 
 */
class GraphElemModule extends ActionModule {
  
	def actionList=List(moveAction,copyAction)	
	
	
	
  def getActionsIterator() = { actionList.iterator }
  
	val secondPointQuestion= Some(new ParamQuestion("nach Punkt",
		Seq(new ParamAnswerDefinition("Endpunkt Verschiebung",DataType.VectorTyp,None))))
		
	val dyQuestion= Some(new ParamQuestion("Eingabe Deltawert",
		Seq(new ParamAnswerDefinition("delta Y: ",DataType.DoubleTyp,None))))	
	
	val moveAction=new ActionIterator("Verschieben",Some(new ParamQuestion("Verschieben von Punkt / Delta",
		Seq(new ParamAnswerDefinition("Startpunkt Verschiebung",DataType.VectorTyp,secondPointQuestion),
			  new ParamAnswerDefinition("oder: Delta X:",DataType.DoubleTyp,dyQuestion)
			))),doMove)
	
	val copyAction=new ActionIterator("Kopieren",Some(new ParamQuestion("Kopieren von Punkt / Delta",
		Seq(new ParamAnswerDefinition("Startpunkt ",DataType.VectorTyp,secondPointQuestion),
			  new ParamAnswerDefinition("oder: Delta X:",DataType.DoubleTyp,dyQuestion)
			))),doCopy)
	
  //val moveAction=new ActionImpl("Verschieben",None,nope)
  
  def nope( data:InstanceData,param:Seq[(String,Constant)]) =  {true}
	
	def doMove(data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {		
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
			  println("move delta:"+delta)
				for(d <-data) {
					
					if(d.ref.typ==TypeInfos.lineTyp) {						
						TransactionManager.tryWriteInstanceField(d.ref,3,d.fieldValue(3).toVector+delta)
						TransactionManager.tryWriteInstanceField(d.ref,4,d.fieldValue(4).toVector+delta)
					}				 
				}
			  
		  true	
		}
		else false
	}
	
	def doCopy(data:Seq[InstanceData],param:Seq[(String,Constant)]) =  {		
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
					if(d.ref.typ==TypeInfos.lineTyp) {
						val inst=TransactionManager.tryCreateInstance(TypeInfos.lineTyp,d.owners,false)
						val instVal=d.clone(inst.ref,d.owners).setField(3,d.fieldValue(3).toVector+delta).setField(4,
							d.fieldValue(4).toVector+delta)						
						TransactionManager.tryWriteInstanceData(instVal)						
					}
				}			
		  true	
		}
		else false
	}
	
	
	
}

class LineModule extends ActionModule {
	val createActionList=List(createLineAction)
	override def getCreateActionsIterator() =  createActionList.iterator
	override def getActionsIterator() =  Seq.empty.iterator
	
	def nextPointQuestion:ParamQuestion=new ParamQuestion("Linie bis Punkt",Seq(
  	new ParamAnswerDefinition("weiterer Punkt",DataType.VectorTyp,None,"LineTo")),true)
  def lineQuestion=new ParamQuestion("Linie erzeugen",Seq(new ParamAnswerDefinition("StartPunkt",DataType.VectorTyp,
  	Some(nextPointQuestion),"Create")))
	
	def createLineAction=new ActionIterator("Linie",Some(lineQuestion),doCreateLine)
	
	def doCreateLine(parents:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean= {
		val parentRef=Array(new OwnerReference(0.toByte,parents.head.ref))
  	//println("create line "+param.mkString)  			
  			for(i <- 0 until param.size-1) {
  				val inst=TransactionManager.tryCreateInstance(TypeInfos.lineTyp,parentRef,false)  				
  				val sp=param(i)._2.toVector
  				TransactionManager.tryWriteInstanceField(inst.ref,3,sp)
  				val ep=param(i+1)._2.toVector
  				TransactionManager.tryWriteInstanceField(inst.ref,4,ep)
  			}		
		true
	}
}

package object TypeInfos {		
	val lineTyp=AllClasses.get.getClassIDByName("LineElem")
	
}