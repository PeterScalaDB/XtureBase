/**
 * Author: Peter Started:22.09.2010
 */
package definition.test

import definition.typ._
import server.storage._
import definition.data._
import definition.expression._
import server.comm.UserSocket

/** a test module for testing the class actions
 * 
 */
class TestActionModule extends ActionModule {
	
	def setObjectType(typeID:Int)= {   	
  }
	
	
	val thirdQuestion=new DialogQuestion("thirdQuestion",Seq(new ParamAnswerDefinition("Groﬂe Zahl eingeben:",DataType.LongTyp,None)))
	val firstQuestion=new DialogQuestion("what int",Seq(new ParamAnswerDefinition("Zahl eingeben:",DataType.IntTyp,None)))
	val oneAction=new ActionImpl("Kopieren",Some(firstQuestion),doOneAction)
	val otherAction=new ActionImpl("Verschieben",Some(new DialogQuestion("what next",Seq(new ParamAnswerDefinition("Zahl eingeben:",DataType.IntTyp,None),
		new ParamAnswerDefinition("Text eingeben:",DataType.StringTyp,Some(thirdQuestion))))),doOtherAction)
	
	val quietAction=new ActionImpl("Gleichbleiben",None,doQuietAction)
	val actionList=List(oneAction,otherAction,quietAction)
	
	def getActionsIterator = actionList.iterator
	
	def doOneAction(u:UserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
		System.out.println("Doing Action One with parameters:"+param.mkString(",")+"data")
		System.out.println(data)
		true
	}
	
	def doOtherAction(u:UserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
		System.out.println("Doing Action two with parameters:"+param.mkString(",")+"data")
		System.out.println(data)
		true
	}
	
	def doQuietAction(u:UserSocket,data:InstanceData,param:Seq[(String,Constant)]):Boolean = {
		System.out.println("Do quiet action "+data)
		u.askEnquiry(thirdQuestion,handleAnswer )
		true
	}
	
	def handleAnswer(u:UserSocket,params:Seq[(String,Constant)]):Unit= {
		if(params.size==0)System.out.println("Stop")
		else{
			System.out.println("Anwsers:"+params)
			u.askEnquiry(firstQuestion,handleAnswer )
		}
		
	}
	
	

}