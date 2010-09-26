/**
 * Author: Peter Started:25.09.2010
 */
package runtime.function

import server.storage.{ActionModule,ActionImpl}
import definition.data._
import definition.typ._
import definition.expression.Constant
import transaction.handling.TransactionManager

/** Action module for base class
 * 
 */
class BaseClassModule extends ActionModule {

	val deleteAction=new ActionImpl("Löschen",Some(new ParamQuestion("Element wirklich löschen ?",
		Seq(new ParamAnswerDefinition("bitte bestätigen:",DataType.BoolTyp,None)))),doDelete)
	
  def getActionCount(): Int = { 1 }

  def getAction(ix: Int): ActionImpl = { if(ix==0) return deleteAction else return null }
  
  def doDelete(data:Seq[InstanceData],param:Seq[Constant]):Boolean = {
  	if(!param.isEmpty && param(0).toBoolean) {
  		TransactionManager.doTransaction {
  		  for(inst <-data)
  		  	TransactionManager.tryDeleteInstance(inst.ref,None)
  		}
  		
  	}  		
  	true
  }

}