/**
 * Author: Peter Started:22.09.2010
 */
package definition.test

import definition.typ._
import server.storage._
import definition.data._
import definition.expression._

/** a test module for testing the class actions
 * 
 */
class TestActionModule extends ActionModule {
	
	val thirdQuestion=new ParamQuestion("thirdQuestion",Seq(new ParamAnswerDefinition("Groﬂe Zahl eingeben:",DataType.LongTyp,None)))
	
	val oneAction=new ActionImpl("Kopieren",Some(new ParamQuestion("what int",Seq(new ParamAnswerDefinition("Zahl eingeben:",DataType.IntTyp,None)))),doOneAction)
	val otherAction=new ActionImpl("Verschieben",Some(new ParamQuestion("what next",Seq(new ParamAnswerDefinition("Zahl eingeben:",DataType.IntTyp,None),
		new ParamAnswerDefinition("Text eingeben:",DataType.StringTyp,Some(thirdQuestion))))),doOtherAction)
	
	val quietAction=new ActionImpl("Gleichbleiben",None,doQuietAction)
	val actionList=List(oneAction,otherAction,quietAction)
	
	def getActionsIterator = actionList.iterator
	
	def doOneAction(data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
		println("Doing Action One with parameters:"+param.mkString(",")+"data")
		println(data)
		true
	}
	
	def doOtherAction(data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
		println("Doing Action two with parameters:"+param.mkString(",")+"data")
		println(data)
		true
	}
	
	def doQuietAction(data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
		println("Do quiet action "+data)
		true
	}
	
	

}