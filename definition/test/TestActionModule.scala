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
	
	val thirdQuestion=new ParamQuestion("thirdQuestion",Seq(new ParamAnswerDefinition(DataType.LongTyp,None)))
	
	val oneAction=new ActionImpl("Löschen",Some(new ParamQuestion("what int",Seq(new ParamAnswerDefinition(DataType.IntTyp,None)))),doOneAction)
	val otherAction=new ActionImpl("Verschieben",Some(new ParamQuestion("what next",Seq(new ParamAnswerDefinition(DataType.IntTyp,None),
		new ParamAnswerDefinition(DataType.StringTyp,Some(thirdQuestion))))),doOtherAction)
	
	def getActionCount:Int={println("TestModule getActionCount" );2}
	
	def getAction(ix:Int):ActionImpl = ix match {
		case 0 => oneAction
		case 1 => otherAction
		case _ => null
	}
	
	def doOneAction(data:Seq[InstanceData],param:Seq[Constant]):Boolean = {
		println("Doing Action One with parameters:"+param.mkString(",")+"data")
		println(data.mkString(","))
		true
	}
	
	def doOtherAction(data:Seq[InstanceData],param:Seq[Constant]):Boolean = {
		println("Doing Action two with parameters:"+param.mkString(",")+"data")
		println(data.mkString(","))
		true
	}
	
	

}