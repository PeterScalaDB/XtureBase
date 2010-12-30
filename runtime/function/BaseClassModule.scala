/**
 * Author: Peter Started:25.09.2010
 */
package runtime.function

import server.storage.{ActionModule,ActionImpl,ActionIterator}
import definition.data._
import definition.typ._
import definition.expression.{Constant,IntConstant}
import transaction.handling.TransactionManager
import server.comm.UserSocket

/** Action module for base class
 * 
 */
class BaseClassModule extends ActionModule {

	val deleteAction=new ActionIterator("L�schen",None,doDelete)
	
	val stressTestAction=new ActionImpl("Stress-Test",ParamQuestion.makeQuestion(List(
		("Stresstest Unterelement-Typ:",("Typ-ID:",DataType.IntTyp)),
		("Stresstest in Property-Feld:",("Feld-Nr:",DataType.IntTyp)),
		("Stresstest wieviele Elemente:",("Anzahl:",DataType.IntTyp))
		)),
		doStressTest)
	
	val mList=List(deleteAction)
	
  def getActionsIterator = mList.iterator 
  
  def doDelete(u:UserSocket,parent:OwnerReference,data:Seq[InstanceData],param:Seq[(String,Constant)]):Boolean = {
  	//if(!param.isEmpty && param(0)._2.toBoolean) {  		
  		  for(inst <-data)
  		  	TransactionManager.tryDeleteInstance(inst.ref,Some(parent),None)  		
  	//}  		
  	true
  }
	
	def doStressTest(u:UserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
  	if(!param.isEmpty) {
  		val typ=param(0)._2.toInt
  		val propField=param(1)._2.toInt
  		val count=param(2)._2.toInt
  		val owner=Array(new OwnerReference(propField.toByte,data.ref))
  		  for(i <- 0 until count)
  		  {
  		  	val inst = TransactionManager.tryCreateInstance(typ,owner,true)
  		  	TransactionManager.tryWriteInstanceField(inst.ref,0,new IntConstant(i))
  		  }
  	}  		
  	true
  }
	
	def setObjectType(typeID:Int)={}

}