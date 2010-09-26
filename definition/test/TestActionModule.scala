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
	
	val thirdQuestion=new ParamQuestion("thirdQuestion",Seq(new ParamAnswerDefinition("Große Zahl eingeben:",DataType.LongTyp,None)))
	
	val oneAction=new ActionImpl("Kopieren",Some(new ParamQuestion("what int",Seq(new ParamAnswerDefinition("Zahl eingeben:",DataType.IntTyp,None)))),doOneAction)
	val otherAction=new ActionImpl("Verschieben",Some(new ParamQuestion("what next",Seq(new ParamAnswerDefinition("Zahl eingeben:",DataType.IntTyp,None),
		new ParamAnswerDefinition("Text eingeben:",DataType.StringTyp,Some(thirdQuestion))))),doOtherAction)
	
	val quietAction=new ActionImpl("Gleichbleiben",None,doQuietAction)
	def getActionCount:Int=3
	
	def getAction(ix:Int):ActionImpl = ix match {
		case 0 => oneAction
		case 1 => otherAction
		case 2 => quietAction
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
	
	def doQuietAction(data:Seq[InstanceData],param:Seq[Constant]):Boolean = {
		println("Do quiet action "+data.map(_.ref).mkString(","))
		true
	}
	
	

}